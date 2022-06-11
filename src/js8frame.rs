use std::fmt;
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, FromRepr};

#[cfg(test)]
use quickcheck::{empty_shrinker, single_shrinker, Arbitrary, Gen};

use crate::coumpound::Compound;

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
            Command::DitDit => " Dit DIT".to_string(),
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

#[derive(PartialEq, Debug)]
pub enum Frame {
    FrameHeartbeat {},
    FrameCQ {},
    FrameDirectedMessage {
        from: Option<Compound>,
        to: Option<Compound>,
    },
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

    use crate::js8frame::Command;
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
}
