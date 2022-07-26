use std::fmt;
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, FromRepr};

#[cfg(test)]
use quickcheck::{empty_shrinker, single_shrinker, Arbitrary, Gen};

#[derive(Clone, FromRepr, EnumIter, PartialEq, Eq, Debug, Hash, Copy)]
#[repr(u32)]
#[allow(non_camel_case_types)]
pub enum BaseGroup {
    Incomplete = 37 * 36 * 10 * 27 * 27 * 27 + 1,
    AllCall,
    JS8Net,

    // Continental dx
    DX_NA, // North America DX group
    DX_SA, // South America DX group
    DX_EU, // Europe DX group
    DX_AS, // Asia DX group
    DX_AF, // Africa DX group
    DX_OC, // Oceania DX group
    DX_AN, // Antarctica DX group

    // itu regions
    REGION_1, // ITU Region 1
    REGION_2, // ITU Region 2
    REGION_3, // ITU Region 3

    // Generic group
    GROUP_0,
    GROUP_1,
    GROUP_2,
    GROUP_3,
    GROUP_4,
    GROUP_5,
    GROUP_6,
    GROUP_7,
    GROUP_8,
    GROUP_9,

    // ops
    COMMAND, // Command group
    CONTROL, // Control group
    NET,     // Net group
    NTS,     // NTS group

    // reserved groups
    RESERVE_0, // Reserved
    RESERVE_1, // Reserved
    RESERVE_2, // Reserved
    RESERVE_3, // Reserved
    RESERVE_4, // Reserved

    // special groups
    APRSIS,   // APRS GROUP
    RAGCHEW,  // RAGCHEW GROUP
    JS8,      // JS8 GROUP
    EMCOMM,   // EMCOMM GROUP
    ARES,     // ARES GROUP
    MARS,     // MARS GROUP
    AMRRON,   // AMRRON GROUP
    RACES,    // RACES GROUP
    RAYNET,   // RAYNET GROUP
    RADAR,    // RADAR GROUP
    SKYWARN,  // SKYWARN GROUP
    CQ,       // CQ GROUP
    HB,       // HB GROUP
    QSO,      // QSO GROUP
    QSOPARTY, // QSO PARTY GROUP
    CONTEST,  // CONTEST GROUP
    FIELDDAY, // FIELD DAY GROUP
    SOTA,     // SOTA GROUP
    IOTA,     // IOTA GROUP
    POTA,     // POTA GROUP
    QRP,      // QRP GROUP
    QRO,      // QRO GROUP
}

impl fmt::Display for BaseGroup {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strval = format!("@{:?}", self).to_uppercase();
        let strval = strval.replace("_", "/");
        write!(f, "{}", strval)
    }
}

impl std::str::FromStr for BaseGroup {
    type Err = ::strum::ParseError;

    fn from_str(s: &str) -> ::core::result::Result<BaseGroup, Self::Err> {
        for callsign in BaseGroup::iter() {
            if s.eq_ignore_ascii_case(&callsign.to_string()) {
                return ::core::result::Result::Ok(callsign);
            }
        }
        ::core::result::Result::Err(::strum::ParseError::VariantNotFound)
    }
}

#[cfg(test)]
impl Arbitrary for BaseGroup {
    fn arbitrary(g: &mut Gen) -> BaseGroup {
        let idx = u32::arbitrary(g);
        let idx = idx % (BaseGroup::QRO as u32 - BaseGroup::Incomplete as u32);
        let idx = idx + (BaseGroup::Incomplete as u32);
        let s = BaseGroup::from_repr(idx);
        s.unwrap()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        if *self == BaseGroup::Incomplete {
            return empty_shrinker();
        }
        single_shrinker(BaseGroup::from_repr(*self as u32 - 1).unwrap())
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum Compound {
    GroupCall { name: String },
    Callsign { base: String, is_portable: bool },
    BaseGroup { kind: BaseGroup },
}

impl Compound {
    pub fn normalize(self) -> Compound { 
        if let Compound::GroupCall { ref name } = self {
            if name.trim().is_empty() {
                return Compound::BaseGroup { kind: BaseGroup::Incomplete };
            }
            for a_basegroup in BaseGroup::iter() {
                if name.trim().eq_ignore_ascii_case(&a_basegroup.to_string().trim()[1..]) {
                    return Compound::BaseGroup { kind: a_basegroup };
                }
            }
        }
        if let Compound::Callsign { ref base, is_portable } = self {
            let base = base.trim();
            if !base.starts_with("3DA0") && base.len() > 6 {
                return Compound::Callsign {base: base[..7].to_string(), is_portable};
            }
        }
        self
    }

    pub fn is_portable(&self) -> bool {
        match self {
            Self::Callsign { base: _, is_portable } => *is_portable,
            _ => false,
        }
    }
}

impl fmt::Display for Compound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            Self::GroupCall { name } => write!(f, "@{}", name),
            Self::Callsign { base, is_portable } => {
                write!(f, "{}{}", base, if *is_portable { "/P" } else { "" })
            }
            Self::BaseGroup { kind } => write!(f, "{}", kind),
        }
    }
}

impl std::str::FromStr for Compound {
    type Err = ::strum::ParseError;

    fn from_str(s: &str) -> ::core::result::Result<Compound, Self::Err> {
        if s.is_empty() {
            return ::core::result::Result::Err(::strum::ParseError::VariantNotFound);
        }
        if s.starts_with("@") {
            // This is a group
            //TODO [@][A-Z0-9\/]{0,3}[\/]?[A-Z0-9\/]{0,3}[\/]?[A-Z0-9\/]{0,3}
            let base_group = BaseGroup::from_str(s);
            if base_group.is_ok() {
                let base_group = base_group.expect("");
                let base_group = Compound::BaseGroup { kind: base_group };
                return ::core::result::Result::Ok(base_group);
            } else {
                let groupcall = s[1..].to_string();
                let groupcall = Compound::GroupCall { name: groupcall };
                return ::core::result::Result::Ok(groupcall);
            }
        } else {
            // This is a callsign
            //TODO check for valid callsign
            let is_portable = s.ends_with("/P");
            let s_len = s.len() - if is_portable { 2 } else { 0};
            let base = s[..s_len].to_string();
            let callsign = Compound::Callsign { base, is_portable };
            return ::core::result::Result::Ok(callsign);
        }
    }
}

#[cfg(test)]
impl Arbitrary for Compound {
    fn arbitrary(g: &mut Gen) -> Self {
        let y = u8::arbitrary(g) % 4;
        match y {
            0 => {
                    let mut valid_characters: Vec<char> = ('A'..'Z').chain('0'..'9').collect();
                    valid_characters.push('/');
                    let valid_characters = &valid_characters[..];
                    let s_size = 1+(u8::arbitrary(g) % 7);
                    let name: String = (0..s_size).map(|_| g.choose(valid_characters).unwrap()).collect();
                    let c = Compound::GroupCall{name};
                    // A groupcall must not be basegroup nor empty
                    c.normalize()
            },
            1 => {
                // [a-zA-Z0-9]{1,3}[0-9][a-zA-Z0-9]{0,3}[a-zA-Z]
                // See https://gist.github.com/JoshuaCarroll/f6b2c64992dfe23feed49a117f5d1a43

                let digits: Vec<char> = ('0'..'9').collect();
                #[allow(non_snake_case)]
                let a_zA_Z: Vec<char> = ('A'..'Z').collect();
                #[allow(non_snake_case)]
                let a_zA_Z0_9: Vec<char> = ('A'..'Z').chain('0'..'9').collect();

                let digits = &digits[..];
                #[allow(non_snake_case)]
                let a_zA_Z = &a_zA_Z[..];
                #[allow(non_snake_case)]
                let a_zA_Z0_9 = &a_zA_Z0_9[..];

                let mut base: Vec<char> = Vec::new();
                let nc = *g.choose(&[1 as usize,2,3]).expect("");
                for _ in 0..nc {

                    let c = *g.choose(a_zA_Z0_9).expect("");
                    base.push(c);
                }
                base.push(*g.choose(digits).expect(""));

                let rnd = u8::arbitrary(g);
                let nc = 1 + rnd % ((6-base.len()) as u8);
                for _ in 0..nc {

                    let c = *g.choose(a_zA_Z0_9).expect("");
                    base.push(c);
                }
                base.push(*g.choose(a_zA_Z).expect(""));
                let base: String = base.iter().collect();
                
        
                Compound::Callsign {
                    base,
                    is_portable: bool::arbitrary(g),
                }.normalize()
            },
            _ => Compound::BaseGroup {
                kind: BaseGroup::arbitrary(g),
            },
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Compound::GroupCall { name } => {
                Box::new(name.shrink().map(|n| Compound::GroupCall { name: n }))
            }
            Self::Callsign { base, is_portable } => {
                // let base = base.clone();
                // let is_portable = *is_portable;
                // let mut chain = empty_shrinker();
                // if is_portable {
                //     chain = single_shrinker(Compound::Callsign {
                //         base: base.clone(),
                //         is_portable: false,
                //     });
                // }
                // let chain = chain.chain(base.shrink().map(move |n| Compound::Callsign {
                //     base: n,
                //     is_portable: is_portable,
                // }));
                // Box::new(chain)
                if *is_portable {
                    single_shrinker(Compound::Callsign {
                                base: base.clone(),
                                is_portable: false,
                            })
                } else {
                    empty_shrinker()
                }
            }
            Self::BaseGroup { kind } => {
                Box::new(kind.shrink().map(|n| Compound::BaseGroup { kind: n }))
            }
            //_ => empty_shrinker(),
        }
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    extern crate quickcheck;

    use std::str::FromStr;
    use strum::IntoEnumIterator;
    use crate::compound::{BaseGroup, Compound};
    use quickcheck::TestResult;

    #[test]
    fn test() {
        assert_eq!("@INCOMPLETE", BaseGroup::Incomplete.to_string());
    }

    #[test]
    fn test_base_group_0() {
        assert_eq!("@GROUP/0", BaseGroup::GROUP_0.to_string());
        assert_eq!(
            BaseGroup::GROUP_0,
            BaseGroup::from_str(&"@GROUP/0").expect("can read group 0")
        );
    }

    #[test]
    fn test_compound_group_0() {
        let group0 = Compound::BaseGroup {
            kind: BaseGroup::GROUP_0,
        };
        assert_eq!("@GROUP/0", group0.to_string());
        assert_eq!(
            group0,
            Compound::from_str(&"@GROUP/0").expect("can read group 0")
        );
    }

    #[test]
    fn test_compound_normalization() {
        for a_basegroup in BaseGroup::iter() {
            assert_eq!(
                Compound::BaseGroup { kind: a_basegroup },
                Compound::GroupCall { name: a_basegroup.to_string()[1..].to_string() }.normalize()
            )
        }
        assert_eq!("@HB", BaseGroup::HB.to_string() );
        assert_eq!("@HB", Compound::BaseGroup { kind: BaseGroup::HB}.to_string() );
        assert_eq!("@HB", Compound::GroupCall { name: "HB".to_string() }.to_string() );
        assert_eq!(
            Compound::BaseGroup { kind: BaseGroup::HB },
            Compound::GroupCall { name: "HB".to_string() }.normalize()
        );
        assert_eq!(
            Compound::BaseGroup { kind: BaseGroup::HB },
            Compound::from_str("@HB").expect("HB is valid group").normalize()
        );
    }

    #[test]
    fn test_compound_callsigns() {
        let c = Compound::Callsign {
            base: "F4LOI".to_string(),
            is_portable: true
        };
        assert_eq!("F4LOI/P", c.to_string());
        assert_eq!(
            c,
            Compound::from_str(&"F4LOI/P").expect("can read portable callsign")
        );
        assert_eq!(
            Compound::BaseGroup {
                kind: BaseGroup::HB
            },
            Compound::from_str(&"@HB").expect("can read group call")
        );
    }

    #[quickcheck]
    fn reparse_basegroup(c: BaseGroup) -> TestResult {
        let cc = BaseGroup::from_str(&c.to_string());
        if let Ok(cc) = cc {
            TestResult::from_bool(c == cc)
        } else {
            TestResult::failed()
        }
    }

    #[quickcheck]
    fn reparse_compound(c: Compound) -> TestResult {
        // only works if group name is not a basegroup

        let cc = Compound::from_str(&c.to_string());
        if let Ok(cc) = cc {
            TestResult::from_bool(c == cc)
        } else {
            TestResult::failed()
        }
    }
}
