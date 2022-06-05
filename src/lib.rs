use regex::Regex;
use crc_all::Crc;

pub const NBASE: u32 = 37 * 36 * 10 * 27 * 27 * 27;
const NTOKENS: u32 = 2063592u32;
const MAX22: u32 = 4194304u32;
const MAXGRID4: u16 = 32400u16;

pub fn char_index(c: char, table_index: u8) -> Option<u32> {
    let table_index = table_index.min(4);
    let i = " 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-./?".find(c)? as u32;
    if table_index == 0 {
        return Some(i);
    } else {
        if i > 36 {
            return None;
        }
        if table_index == 1 {
            return Some(i);
        } else if table_index == 2 && i > 0 {
            return Some(i - 1);
        } else if table_index == 3 {
            if c.is_digit(10) {
                return Some(i - 1);
            }
        } else if table_index == 4 {
            if i == 0 {
                return Some(0);
            }
            if i > 10 {
                return Some(i - 10);
            }
        }
    }
    return None;
}

/// Pack a valid callsign into a 28-bits integer
pub fn pack28(callsign: &str) -> Option<u32> {
    if callsign.len() == 0 {
        return None;
    }
    assert!(callsign.is_ascii());
    let mut callsign = callsign;
    let mut local_callsign: String;

    if callsign.starts_with("3DA0") {
        local_callsign = "3D0".to_owned() + &callsign[4..7];
        callsign = &local_callsign;
    } else if callsign.starts_with("3X") && callsign.chars().nth(2).unwrap().is_alphabetic() {
        local_callsign = "Q".to_owned() + &callsign[2..7];
        callsign = &local_callsign;
    } else if callsign.starts_with("CQ ") {
        if callsign.len() >= 6 && callsign[3..6].chars().all(|c| c.is_digit(10)) {
            println!("{:?} {:?}", callsign, &callsign[3..6]);
            let nfreq = callsign[3..6].parse::<u32>().unwrap();
            return Some(NBASE + 3 + nfreq);
        }
        return Some(NBASE + 1);
    } else if callsign.starts_with("DE ") {
        return Some(267796945);
    } else if callsign.starts_with("QRZ ") {
        return Some(NBASE + 2);
    }

    let callsign_length = if let Some(l) = callsign.find(' ') {
        l
    } else {
        callsign.len()
    };
    if callsign_length <= 6 {
        if callsign_length >= 3 && callsign.chars().nth(2).unwrap().is_digit(10) {
            local_callsign = "".to_owned() + callsign + " ";
            callsign = &local_callsign;
        } else if callsign_length >= 2
            && callsign.chars().nth(1).unwrap().is_digit(10)
            && callsign_length <= 5
        {
            local_callsign = " ".to_owned() + callsign;
            callsign = &local_callsign;
        }
    }
    let mut indexes = callsign
        .chars()
        .enumerate()
        .map(|(i, c)| char_index(c, 1 + i as u8))
        .collect::<Vec<Option<u32>>>();
    indexes.resize(6, None);
    // println!("{:?} -> {:?}", callsign, indexes);
    let n28 = indexes[0]?;
    let n28 = n28 * 36 + indexes[1]?;
    let n28 = n28 * 10 + indexes[2]?;
    let n28 = n28 * 27 + indexes[3]?;
    let n28 = n28 * 27 + indexes[4]?;
    let n28 = n28 * 27 + indexes[5]?;
    return Some(NTOKENS + MAX22 + n28);
}

/// Converts Maidenhead grid locator to degrees of **West** longitude
/// and North latitude.
pub fn grid2deg(locator: &str) -> Option<(f32, f32)> {
    let mut locator_overload: String;
    let mut locator = locator;
    if !locator.is_ascii() {
        return None;
    }
    if locator.len() < 6 {
        locator_overload = locator.to_owned() + "mm";
        locator = &locator_overload;
    }
    assert!(locator.len() >= 6);
    let loc = locator.to_uppercase();
    let locator = loc.as_bytes();
    let nlong = 180.0 - 20.0 * (locator[0] - b'A') as f32;
    let n20d = 2.0 * (locator[2] - b'0') as f32;
    let xminlong = 5.0 * ((locator[4] - b'A') as f32 + 0.5);
    let dlong = nlong - n20d - xminlong / 60.0;
    let nlat = -90.0 + 10.0 * (locator[1] - b'A') as f32 + (locator[3] - b'0') as f32;
    let xminlat = 2.5 * (locator[5] - b'A') as f32 + 0.5;
    let dlat = nlat + xminlat / 60.0;
    return Some((dlong, dlat));
}

#[derive(Debug, PartialEq)]
pub enum GridError {
    FreeText,
    Invalid,
}

pub const NGBASE: u16 = 180 * 180;
pub fn pack_grid_or_report(callsign: &str) -> Result<u16, GridError> {
    let mut callsign = callsign;
    let mut local_callsign: String;
    if callsign.starts_with("-") {
        let report = callsign[1..3].parse::<u16>().unwrap();
        if 1 <= report && report <= 30 {
            return Ok(NGBASE + 1 + report);
        }
    } else if callsign.starts_with("R-") {
        let report = callsign[2..4].parse::<u16>().unwrap();
        if 1 <= report && report <= 30 {
            return Ok(NGBASE + 31 + report);
        }
    } else if callsign.starts_with("RO  ") {
        return Ok(NGBASE + 62);
    } else if callsign.starts_with("RRR ") {
        return Ok(NGBASE + 63);
    } else if callsign.starts_with("73  ") {
        return Ok(NGBASE + 64);
    } else if callsign.starts_with("    ") {
        return Ok(NGBASE + 1);
    }
    let mut n = 99;
    if let Ok(v) = callsign.parse::<i32>() {
        n = v;
    } else {
        if let Ok(v) = callsign[1..4].parse::<i32>() {
            n = v;
        } else {
        }
    }
    if -50 <= n && n <= 49 {
        if callsign.starts_with("R") {
            local_callsign = format!("LA{}", n + 50);
            callsign = &local_callsign;
        } else {
            local_callsign = format!("KA{}", n + 50);
            callsign = &local_callsign;
        }
    } else {
        // Check for free text
        let grid = callsign.as_bytes();
        if grid[0] < b'A'
            || grid[0] > b'R'
            || grid[1] < b'A'
            || grid[1] > b'R'
            || grid[2] < b'0'
            || grid[2] > b'9'
            || grid[3] < b'0'
            || grid[3] > b'R'
        {
            return Err(GridError::FreeText);
        }
    }
    let locator = "".to_owned() + callsign + "mm";
    let (long, lat) = grid2deg(&locator).expect("invaid grid locator");
    let long = long as i16;
    let lat = (lat + 90.0) as i16;
    let ng = ((long + 180) / 2) * 180 + lat + 90;
    assert!(ng >= 0);
    return Ok(ng as u16);
}

/// Convert in uppercase and merge multiple blanks into one.
pub fn fmtmsg(msg: &str) -> String {
    let regex = Regex::new(r"\s+").unwrap();
    let msg = msg.to_uppercase();
    let msg = regex.replace_all(&msg, " ");
    msg.into_owned()
}

/// Pack a JT4/JT9/JT65 message into twelve 6-bits symbols + 3 bits for type,
/// ie 75 bits
pub fn pack77(msg: &str) -> Option<[u8; 10]> {
    let delimiters_index: Vec<_> = msg.match_indices(char::is_whitespace).collect();
    let first_space_index = delimiters_index[0].0 + 1;
    let second_space_index = delimiters_index.get(1);
    let n28a = pack28(&msg[..first_space_index])?;
    let n28b = pack28(&msg[first_space_index..])?;
    let i3: u8 = 1;
    let igrid4: u16;
    if let Some(s2) = second_space_index {
        igrid4 = pack_grid_or_report(&msg[s2.0+1..]).ok()?;
    } else {
        igrid4 = MAXGRID4 + 1;
    }
    let n28a = n28a << 1;
    let n28b = n28b << 1;
    let mut result: [u8; 10] = [0; 10];
    result[0] = (n28a >> 21) as u8;
    result[1] = (n28a >> 13) as u8;
    result[2] = (n28a >> 5) as u8;
    result[3] = ((n28a << 3) | (n28b >> 26)) as u8;
    result[4] = (n28b >> 18) as u8;
    result[5] = (n28b >> 10) as u8;
    result[6] = (n28b >> 2) as u8;
    result[7] = (n28b << 6) as u8 | (igrid4 >> 10) as u8;
    result[8] = (igrid4 >> 2) as u8;
    result[9] = (igrid4 << 6) as u8| (i3 << 3);
    Some(result)
}

pub fn crc12(data: &[u8]) -> u16 {
    let mut crc12 = Crc::<u16>::new(0xc06, 12, 0x00, 0x00, false);
    crc12.update(data)
}

// pub fn crc14(data: &[u8]) -> u16 {
//     let mut crc14 = Crc::<u16>::new(0x2757, 14, 0x00, 0x00, false);
//     crc14.update(data)
// }

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

#[cfg(test)]
mod tests {
    use crate::{char_index, fmtmsg, grid2deg, GridError};
    use crate::{pack28, pack_grid_or_report, pack77, crc12};
    use crate::{NBASE, NGBASE};
    use quickcheck::TestResult;

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn char_index_space() {
        assert_eq!(
            " 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-./?".find(' '),
            Some(0)
        );
        assert_eq!(char_index(' ', 0), Some(0));
        assert_eq!(char_index(' ', 1), Some(0));
        assert_eq!(char_index(' ', 2), None);
        assert_eq!(char_index(' ', 3), None);
        assert_eq!(char_index(' ', 4), Some(0));
    }

    #[test]
    fn char_index_digit() {
        for (i, c) in "0123456789".chars().enumerate() {
            assert_eq!(char_index(c, 0), Some((1 + i) as u32));
            assert_eq!(char_index(c, 1), Some((1 + i) as u32));
            assert_eq!(char_index(c, 2), Some(i as u32));
            assert_eq!(char_index(c, 3), Some(i as u32));
            assert_eq!(char_index(c, 4), None);
        }
    }

    #[test]
    fn char_index_alpha() {
        for (i, c) in "ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars().enumerate() {
            assert_eq!(char_index(c, 0), Some((11 + i) as u32));
            assert_eq!(char_index(c, 1), Some((11 + i) as u32));
            assert_eq!(char_index(c, 2), Some((10 + i) as u32));
            assert_eq!(char_index(c, 3), None);
            assert_eq!(char_index(c, 4), Some(1 + i as u32));
        }
    }
    #[test]
    fn char_index_others() {
        for (i, c) in "+-./?".chars().enumerate() {
            assert_eq!(char_index(c, 0), Some((37 + i) as u32));
            assert_eq!(char_index(c, 1), None);
            assert_eq!(char_index(c, 2), None);
            assert_eq!(char_index(c, 3), None);
            assert_eq!(char_index(c, 4), None);
        }
    }

    #[test]
    fn pack28_special() {
        assert_eq!(pack28("DE "), Some(267796945));
        assert_eq!(pack28("QRZ "), Some(NBASE + 2));
        assert_eq!(pack28("CQ "), Some(NBASE + 1));
    }

    #[quickcheck]
    fn pack28_cq_freq(freq: u16) -> TestResult {
        if freq > 999 {
            return TestResult::discard();
        }
        let s = format!("CQ {:03}", freq);
        TestResult::from_bool(pack28(&s) == Some(NBASE + 3 + (freq as u32)))
    }

    #[quickcheck]
    fn pack28_swaziland(c1: char, c2: char, c3: char) -> TestResult {
        if !c1.is_alphabetic() || !c2.is_alphabetic() || !c2.is_alphabetic() {
            return TestResult::discard();
        }
        let callsign = format!("3DA0{}{}{}", c1, c2, c3);
        if !callsign.is_ascii() {
            return TestResult::discard();
        }
        let same_as = format!("3D0{}{}{}", c1, c2, c3);
        TestResult::from_bool(pack28(&callsign) == pack28(&same_as))
    }

    #[test]
    fn pack28_guinea() {
        assert_eq!(pack28("3XA0XYZ"), pack28("QA0XYZ"));
    }

    #[test]
    fn pack28_known() {
        assert_eq!(pack28(""), None);
        assert_eq!(pack28(" "), None);
        assert_eq!(pack28("ABC"), None);
        assert_eq!(pack28("LL3AJG"), Some(166340741u32));
        assert_eq!(pack28("L0ABC"), Some(10392112u32));
        assert_eq!(pack28("LL3JG"), Some(166347214u32));
    }

    #[test]
    fn packgrid_locator() {
        assert_eq!(pack_grid_or_report("JN38mm"), Ok(15708));
        assert_eq!(pack_grid_or_report("JN38"), Ok(15708));
    }

    #[test]
    fn packgrid_special() {
        assert_eq!(pack_grid_or_report("RO  "), Ok(NGBASE + 62));
        assert_eq!(pack_grid_or_report("RRR "), Ok(NGBASE + 63));
        assert_eq!(pack_grid_or_report("73  "), Ok(NGBASE + 64));
        assert_eq!(pack_grid_or_report("    "), Ok(NGBASE + 1));
    }

    #[test]
    fn packgrid_freetext() {
        assert_eq!(pack_grid_or_report("UVBA"), Err(GridError::FreeText));
        assert_eq!(pack_grid_or_report("AAUV"), Err(GridError::FreeText));
    }

    #[quickcheck]
    fn packgrid_R_qc(n: u8) -> TestResult {
        if n < 1 || n > 30 {
            return TestResult::discard();
        }
        let grid = format!("R-{:02}", n);
        TestResult::from_bool(pack_grid_or_report(&grid) == Ok(NGBASE + 31 + n as u16))
    }

    #[quickcheck]
    fn packgrid_signal_report_qc(n: u8) -> TestResult {
        if n < 1 || n > 30 {
            return TestResult::discard();
        }
        let grid = format!("-{:02}", n);
        TestResult::from_bool(pack_grid_or_report(&grid) == Ok(NGBASE + 1 + n as u16))
    }

    #[test]
    fn grid2deg_test() {
        let actual_mm = grid2deg("JN38mm").expect("expect Some");
        assert!((actual_mm.0 - (-7.0)) < 0.1);
        assert!((actual_mm.1 - (48.5)) < 0.1);
        let actual = grid2deg("JN38");
        assert_eq!(actual, Some(actual_mm));
    }

    #[test]
    fn fmtmsg_test() {
        assert_eq!(fmtmsg("aBcDef"), "ABCDEF");
        assert_eq!(fmtmsg("aBc Def"), "ABC DEF");
        assert_eq!(fmtmsg("aBc  Def"), "ABC DEF");
        assert_eq!(fmtmsg("aBc   Def"), "ABC DEF");
        assert_eq!(fmtmsg("aBc \tDef"), "ABC DEF");
    }

    #[test]
    fn pack77_test() {
        assert_eq!(
            pack77("CQ DL1ABC JO62"),
            Some([250, 8, 49, 147, 68, 74, 17, 142, 209, 8])
        );
    }

    #[test]
    fn crc_test() {
        let data = pack77("CQ DL1ABC JO62").unwrap();
        let crc = crc12(&data);
        assert_eq!(
            crc,
            2688
        );
    }
}
