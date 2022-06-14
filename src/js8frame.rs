use lazy_static::lazy_static;
use regex::Regex;
use std::fmt;
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, FromRepr};

#[cfg(test)]
use quickcheck::{empty_shrinker, single_shrinker, Arbitrary, Gen};

use crate::{compound::Compound, js8frame};

// submode types
#[derive(PartialEq, Eq, Debug)]
pub enum SubmodeType {
    JS8CallNormal = 0,
    JS8CallFast = 1,
    JS8CallTurbo = 2,
    JS8CallSlow = 4,
    JS8CallUltra = 8,
}

#[derive(Clone, FromRepr, EnumIter, PartialEq, Eq, Debug, Hash, Copy)]
#[repr(i32)]
#[allow(non_camel_case_types)]
pub enum Command {
    SnrQuestion = 0,
    DitDit,
    Nack,
    HearingQuestion,
    GridQuestion,
    Relay,
    StatusQuestion,
    Status,
    Hearing,
    Msg,
    MsgTo,
    Query,
    QueryMsgs,
    QueryCall,
    Ack,
    Grid,
    InfoQuestion,
    Info,
    FineBusiness,
    HowCopyQuestion,
    EndOfContact,
    RogerRoger,
    QslQuestion,
    Qsl,
    Cmd,
    Snr,
    No,
    Yes,
    BestRegards,
    HearbeatSnr, // (was ACK in 2.1, now deprecated)
    AgainQuestion,
    FreeText,
}

impl Command {
    pub fn crc_size(&self) -> Option<usize> {
        match *self {
            Command::Relay => Some(16),
            Command::Msg => Some(16),
            Command::MsgTo => Some(16),
            Command::QueryMsgs => Some(16),
            Command::QueryCall => Some(16),
            Command::Grid => Some(0),
            Command::HowCopyQuestion => Some(16),
            Command::Cmd => Some(16),
            _ => None,
        }
    }

    pub fn may_include_snr(&self) -> bool {
        match *self {
            Command::Snr => true,
            Command::HearbeatSnr => true,
            _ => false,
        }
    }

    pub fn should_be_buffered(&self) -> bool {
        //{5, 9, 10, 11, 12, 13, 15, 24}
        match *self {
            Command::Relay => true,
            Command::Msg => true,
            Command::MsgTo => true,
            Command::Query => true,
            Command::QueryMsgs => true,
            Command::QueryCall => true,
            Command::Grid => true,
            Command::Cmd => true,
            _ => false,
        }
    }

    pub fn is_autoreply(&self) -> bool {
        //{0, 2, 3, 4, 6, 9, 10, 11, 12, 13, 14, 16, 30}
        match *self {
            Command::Snr => true,
            Command::Nack => true,
            Command::HearingQuestion => true,
            Command::GridQuestion => true,
            Command::StatusQuestion => true,
            Command::Msg => true,
            Command::MsgTo => true,
            Command::Query => true,
            Command::QueryMsgs => true,
            Command::QueryCall => true,
            Command::Ack => true,
            Command::InfoQuestion => true,
            Command::AgainQuestion => true,
            _ => false,
        }
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strval = match *self {
            Command::DitDit => " DIT DIT".to_string(),
            Command::Relay => ">".to_string(),
            Command::MsgTo => " MSG TO:".to_string(),
            Command::QueryMsgs => " QUERY MSGS".to_string(),
            Command::QueryCall => " QUERY CALL".to_string(),
            Command::HowCopyQuestion => " HW CPY?".to_string(),
            Command::EndOfContact => " SK".to_string(),
            Command::RogerRoger => " RR".to_string(),
            Command::BestRegards => " 73".to_string(),
            Command::HearbeatSnr => " HEARTBEAT SNR".to_string(),
            Command::AgainQuestion => " AGN?".to_string(),
            Command::FreeText => " ".to_string(),
            Command::FineBusiness => " FB".to_string(),
            _ => format!(" {:?}", self)
                .replace("Question", "?")
                .to_ascii_uppercase(),
        };
        write!(f, "{}", strval)
    }
}

impl std::str::FromStr for Command {
    type Err = ::strum::ParseError;

    fn from_str(s: &str) -> ::core::result::Result<Self, Self::Err> {
        for cmd in Command::iter() {
            if s.eq_ignore_ascii_case(&cmd.to_string()) {
                return ::core::result::Result::Ok(cmd);
            }
        }
        if s.eq_ignore_ascii_case("  ") {
            return Ok(Command::FreeText);
        }
        if s.eq_ignore_ascii_case(" QUERY MSGS?") {
            return Ok(Command::QueryMsgs);
        }
        if s.eq_ignore_ascii_case("?") {
            return Ok(Command::SnrQuestion);
        }
        ::core::result::Result::Err(::strum::ParseError::VariantNotFound)
    }
}

#[cfg(test)]
impl Arbitrary for Command {
    fn arbitrary(g: &mut Gen) -> Command {
        let cmds: Vec<Command> = Command::iter().collect();
        let cmds = &cmds[..];
        *g.choose(cmds).expect("")
    }
}

#[derive(PartialEq, Eq, Debug, FromRepr)]
#[repr(u8)]
pub enum FrameType {
    FrameHeartbeatOrCQ = 0b000,
    FrameCoumpound = 0b001,
    FrameCoumpoundDirected = 0b010,
    FrameDirected = 0b011,
    FrameData = 0b100,           // actually 0b10x
    FrameDataCompressed = 0b110, // actually 0b11x
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum Frame {
    FrameHeartbeat {},
    FrameCQ {},
    FrameDirectedMessage {
        from: Option<Compound>,
        to: Option<Compound>,
        cmd: Option<Command>,
    },
}

impl Frame {
    pub fn is_valid(&self) -> bool {
        match self {
            Frame::FrameDirectedMessage {
                from: _,
                to: None,
                cmd: _,
            } => false,
            Frame::FrameDirectedMessage {
                from: _,
                to: Some(Compound::GroupCall { name }),
                cmd: _,
            } => !name.is_empty(),
            _ => true,
        }
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Frame::FrameDirectedMessage { from, to, cmd } => {
                let mut s: Vec<String> = Vec::new();
                let mut has_from = false;
                if let Some(from) = from {
                    has_from = true;
                    s.push(format!("{}:", from));
                }
                if let Some(to) = to {
                    if has_from {
                        s.push(" ".to_string());
                    }
                    s.push(format!("{}", to));
                }
                if let Some(cmd) = cmd {
                    s.push(cmd.to_string());
                }
                let s = s.join("");
                write!(f, "{}", s)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

impl std::str::FromStr for Frame {
    type Err = ::strum::ParseError;

    fn from_str(s: &str) -> ::core::result::Result<Self, Self::Err> {
        static callsign_pattern: &str = "(?P<callsign>[@]?[A-Z0-9/]+)";
        static optional_cmd_pattern: &str = "(?P<cmd>\\s?(?:AGN[?]|QSL[?]|HW CPY[?]|MSG TO[:]|SNR[?]|INFO[?]|GRID[?]|STATUS[?]|QUERY MSGS[?]|HEARING[?]|(?:(?:STATUS|HEARING|QUERY CALL|QUERY MSGS|QUERY|CMD|MSG|NACK|ACK|73|YES|NO|HEARTBEAT SNR|SNR|QSL|RR|SK|FB|INFO|GRID|DIT DIT))|[?> ]))?";
        static optional_num_pattern: &str = "(?P<num>\\s?[-+]?(?:3[01]|[0-2]?[0-9]))?";

        lazy_static! {
            static ref directed_frame_pattern: String =
                "^".to_owned() + callsign_pattern + optional_cmd_pattern + optional_num_pattern;
            static ref directed_re: Regex = Regex::new(&directed_frame_pattern).unwrap();
            static ref callsign_re: Regex = Regex::new(callsign_pattern).unwrap();
            static ref optional_cmd_re: Regex = Regex::new(optional_cmd_pattern).unwrap();
            static ref optional_num_re: Regex = Regex::new(optional_num_pattern).unwrap();
            static ref optional_grid_re: Regex =
                Regex::new("(?<grid>\\s?[A-R]{2}[0-9]{2})?").unwrap();
            static ref optional_extended_grid_re: Regex =
                Regex::new("^(?<grid>\\s?(?:[A-R]{2}[0-9]{2}(?:[A-X]{2}(?:[0-9]{2})?)*))?")
                    .unwrap();
        }
        if let Some(x) = directed_re.captures(s) {
            let mut the_cmd: Option<Command> = None;
            let mut the_callsign: Option<Compound> = None;
            if let Some(callsign) = x.name("callsign") {
                let callsign = callsign.as_str();
                if let Ok(callsign) = Compound::from_str(callsign) {
                    the_callsign = Some(callsign);
                }
            }
            if let Some(cmd) = x.name("cmd") {
                let cmd = cmd.as_str();
                if let Ok(cmd) = Command::from_str(cmd) {
                    the_cmd = Some(cmd);
                }
            }
            let a_frame = Frame::FrameDirectedMessage {
                from: None,
                to: the_callsign,
                cmd: the_cmd,
            };
            return Ok(a_frame);
        }
        ::core::result::Result::Err(::strum::ParseError::VariantNotFound)
    }
}

#[cfg(test)]
impl Arbitrary for Frame {
    fn arbitrary(g: &mut Gen) -> Frame {
        let mut the_callsign: Option<Compound> = None;
        if bool::arbitrary(g) {
            the_callsign = Some(Compound::arbitrary(g));
        }
        let mut the_cmd: Option<Command> = None;
        if bool::arbitrary(g) {
            the_cmd = Some(Command::arbitrary(g));
        }
        Frame::FrameDirectedMessage {
            from: None,
            to: the_callsign,
            cmd: the_cmd,
        }
    }
}

/// frame type transmitted via itype and decoded by the ft8 decoded
#[derive(PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum TransmissionType {
    JS8Call = 0b000,      // <- any other frame of the message
    JS8CallFirst = 0b001, // <- the first frame of a message
    JS8CallLast = 0b010,  // <- the last frame of a message
    JS8CallData = 0b100,  // <- flagged frame (no frame type header)
}

#[derive(PartialEq, Debug)]
pub struct FrameTransmission {
    frame: Frame,
    kind: TransmissionType,
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    extern crate quickcheck;

    use std::str::FromStr;

    use crate::{
        compound::{Compound, BaseGroup},
        js8frame::{Command, Frame},
    };
    use quickcheck::TestResult;

    #[test]
    fn test_cmd() {
        assert_eq!(" HW CPY?", Command::HowCopyQuestion.to_string());
        assert_eq!(" QSL", Command::Qsl.to_string());
        assert_eq!(" QSL?", Command::QslQuestion.to_string());
    }

    #[test]
    fn test_cmd_special() {
        assert_eq!(
            Command::FreeText,
            Command::from_str(&" ").expect("as free text")
        );
        assert_eq!(
            Command::FreeText,
            Command::from_str(&" ").expect("as free text")
        );
        assert_eq!(
            Command::QueryMsgs,
            Command::from_str(&" QUERY MSGS?").expect("as query msgs")
        );
        assert_eq!(
            Command::QueryMsgs,
            Command::from_str(&" QUERY MSGS").expect("as query msgs")
        );
        assert_eq!(
            Command::Relay,
            Command::from_str(&">").expect("as relay message")
        );
        assert_eq!(
            Command::FineBusiness,
            Command::from_str(&" FB").expect("as fine business")
        );
        assert_eq!(
            Command::AgainQuestion,
            Command::from_str(&" AGN?").expect("as fine business")
        );
        assert_eq!(
            Command::SnrQuestion,
            Command::from_str(&"?").expect("as query SNR compatibiltiy")
        );
    }

    #[quickcheck]
    fn reparse_command(c: Command) -> TestResult {
        let cc = Command::from_str(&c.to_string());
        if let Ok(cc) = cc {
            TestResult::from_bool(c == cc)
        } else {
            TestResult::failed()
        }
    }

    #[test]
    fn test_direct_frame() {
        let f = Frame::FrameDirectedMessage {
            from: None,
            to: Some(Compound::from_str("J1Y").expect("valid callsign")),
            cmd: Some(Command::Ack),
        };
        assert_eq!("J1Y ACK", f.to_string());
        assert_eq!(
            f,
            Frame::from_str(&"J1Y ACK").expect("J1Y ACK is a valid message")
        );

        assert_eq!(
            Frame::FrameDirectedMessage {
                from: None,
                to: Some(Compound::from_str("J1Y").expect("valid callsign")),
                cmd: Some(Command::SnrQuestion),
            },
            Frame::from_str(&"J1Y?").expect("J1Y? is a valid message")
        );

        assert_eq!(
            Frame::FrameDirectedMessage {
                from: None,
                to: Some(Compound::Callsign {
                    base: "J1Y".to_string(),
                    is_portable: true
                }),
                cmd: None,
            },
            Frame::from_str(&"J1Y/P").expect("J1Y! HELLO WORLD is a valid message")
        );

        assert_eq!(
            "J1Y/P NO",
            Frame::FrameDirectedMessage {
                from: None,
                to: Some(Compound::Callsign {
                    base: "J1Y".to_string(),
                    is_portable: true
                }),
                cmd: Some(Command::No),
            }
            .to_string(),
        );

        let f = Frame::FrameDirectedMessage {
            from: None,
            to: Some(Compound::Callsign {
                base: "A4XP".to_string(),
                is_portable: true,
            }),
            cmd: Some(Command::Msg),
        };
        assert_eq!("A4XP/P MSG", f.to_string());
        assert_eq!(f, Frame::from_str(&f.to_string()).unwrap());

        let f = Frame::FrameDirectedMessage {
            from: None,
            to: Some(Compound::BaseGroup {
                kind: crate::compound::BaseGroup::AllCall,
            }),
            cmd: None,
        };
        assert_eq!("@ALLCALL", f.to_string());
        assert_eq!(f, Frame::from_str(&f.to_string()).unwrap());

        let f = Frame::FrameDirectedMessage {
            from: None,
            to: Some(Compound::GroupCall {
                name: "AAA".to_string(),
            }),
            cmd: Some(Command::Relay),
        };
        assert_eq!("@AAA>", f.to_string());

        assert_eq!(
            Frame::FrameDirectedMessage {
                from: None,
                to: Some(Compound::BaseGroup { kind: BaseGroup::SOTA }),
                cmd: Some(Command::DitDit),
            },
            Frame::from_str(&"@SOTA DIT DIT").expect("@SOTA DIT DIT is a valid message")
        );

        assert_eq!(f, Frame::from_str(&f.to_string()).unwrap());

        //
        assert_eq!(
            Frame::FrameDirectedMessage {
                from: None,
                to: Some(Compound::BaseGroup { kind: BaseGroup::AllCall }),
                cmd: Some(Command::FreeText),
            },
            Frame::from_str(&"@ALLCALL HELLO NET PSE QSY 14300").expect("@ALLCALL HELLO NET PSE QSY 14300 is a valid message")
        );

        // assert_eq!(
        //     Frame::FrameDirectedMessage {
        //         from: None,
        //         to: Some(Compound::from_str("J1Y").expect("valid callsign")),
        //         cmd: Some(Command::FreeText),
        //     },
        //     Frame::from_str(&"J1Y! HELLO WORLD").expect("J1Y! HELLO WORLD is a valid message")
        // );

        // assert_eq!(
        //     Frame::FrameDirectedMessage {
        //         from: None,
        //         to: Some(Compound::from_str("J1Y").expect("valid callsign")),
        //         cmd: Some(Command::FreeText),
        //     },
        //     Frame::from_str(&"J1Y! HELLO WORLD").expect("J1Y! HELLO WORLD is a valid message")
        // );
    }

    #[quickcheck]
    fn reparse_frame(c: Frame) -> TestResult {
        if !c.is_valid() {
            return TestResult::discard();
        }
        let cc = Frame::from_str(&c.to_string());
        if let Ok(cc) = cc {
            TestResult::from_bool(c == cc)
        } else {
            TestResult::failed()
        }
    }
}
