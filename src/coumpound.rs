use std::fmt;
use strum::IntoEnumIterator;
use strum_macros::{EnumIter, EnumString, FromRepr};

#[cfg(test)]
use quickcheck::{empty_shrinker, single_shrinker, Arbitrary, Gen};

#[derive(Clone, FromRepr, EnumIter, PartialEq, Debug, Hash, Copy)]
#[repr(u32)]
#[allow(non_camel_case_types)]
pub enum BaseCallSign {
    Incomplete = 37 * 36 * 10 * 27 * 27 * 27,
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

impl fmt::Display for BaseCallSign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strval = format!("@{:?}", self);
        let strval = strval.replace("_", "/");
        write!(f, "{}", strval)
    }
}

impl std::str::FromStr for BaseCallSign {
    type Err = ::strum::ParseError;

    fn from_str(s: &str) -> ::core::result::Result<BaseCallSign, Self::Err> {
        for callsign in BaseCallSign::iter() {
            if s.eq_ignore_ascii_case(&callsign.to_string()) {
                return ::core::result::Result::Ok(callsign);
            }
        }
        ::core::result::Result::Err(::strum::ParseError::VariantNotFound)
    }
}

#[cfg(test)]
impl Arbitrary for BaseCallSign {
    fn arbitrary(g: &mut Gen) -> BaseCallSign {
        let idx = u32::arbitrary(g);
        let idx = idx % (BaseCallSign::QRO as u32 - BaseCallSign::Incomplete as u32);
        let idx = idx + (BaseCallSign::Incomplete as u32);
        let s = BaseCallSign::from_repr(idx);
        s.unwrap()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        if *self == BaseCallSign::Incomplete {
            return empty_shrinker();
        }
        single_shrinker(BaseCallSign::from_repr(*self as u32 - 1).unwrap())
    }
}

#[derive(PartialEq, Debug)]
pub enum Compound {
    GroupCall { name: String },
    Callsign { base: String, is_portable: bool },
    BaseCallsign { kind: BaseCallSign },
}

impl fmt::Display for Compound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            Self::GroupCall { name } => write!(f, "@{}", name),
            Self::Callsign { base, is_portable } => {
                write!(f, "{}{}", base, if *is_portable { "/P" } else { "" })
            }
            Self::BaseCallsign { kind } => write!(f, "{}", kind),
        }
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    extern crate quickcheck;

    use std::str::FromStr;

    use crate::coumpound::{BaseCallSign, Compound};
    use quickcheck::TestResult;

    #[test]
    fn test() {
        assert_eq!("@Incomplete", BaseCallSign::Incomplete.to_string());
    }

    #[test]
    fn test_group_0() {
        assert_eq!("@GROUP/0", BaseCallSign::GROUP_0.to_string());
        assert_eq!(
            BaseCallSign::GROUP_0,
            BaseCallSign::from_str(&"@GROUP/0").expect("can read group 0")
        );
    }

    #[quickcheck]
    fn reparse_basecallsign(c: BaseCallSign) -> TestResult {
        let cc = BaseCallSign::from_str(&c.to_string());
        if let Ok(cc) = cc {
            TestResult::from_bool(c == cc)
        } else {
            TestResult::failed()
        }
    }
}
