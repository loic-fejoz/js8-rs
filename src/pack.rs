use std::str::FromStr;

use crate::compound::{BaseGroup, Compound};
use crate::js8frame::Frame;
use bitvec::prelude::*;
use crc_all::Crc;
use regex::Regex;
use strum::IntoEnumIterator;

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

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct Js8PackedCompound(BitArr!(for 28, in u32, Msb0));

impl From<Js8PackedCompound> for u32 {
    fn from(Js8PackedCompound(bits): Js8PackedCompound) -> u32 {
        bits.load::<u32>()
    }
}

impl From<u32> for Js8PackedCompound {
    fn from(value: u32) -> Js8PackedCompound {
        let v = BitArray::new([value]);
        Js8PackedCompound(v)
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct DenormalizedCompound(Compound);

pub struct JS8Protocol {}

impl JS8Protocol {
    pub const NBASECALL: u32 = 37 * 36 * 10 * 27 * 27 * 27;
    const ALPHANUMERIC: &'static str = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ /@"; // callsign and grid alphabet

    /// Trick and adapt for some country
    /// Make sure it is at least 6 letters long including spaces if not long enough
    /// See pack_callsign_str
    fn denormalize(callsign: Compound) -> DenormalizedCompound {
        let callsign = callsign.normalize();
        if let Compound::Callsign {
            ref base,
            is_portable: p,
        } = callsign
        {
            let mut callsign = base.to_string();
            if callsign.len() < 8 {
                callsign = callsign + "        ";
            }
            if callsign.starts_with("3DA0") {
                // workaround for swaziland
                callsign = "3D0".to_string() + &callsign[4..];
            } else if callsign.starts_with("3X")
                && callsign.chars().nth(2).unwrap().is_ascii_alphabetic()
            {
                // workaround for guinea
                callsign = "Q".to_string() + &callsign[2..];
            }
            if !callsign.chars().nth(2).unwrap().is_ascii_digit() {
                callsign = " ".to_string() + &callsign;
            }
            let callsign = callsign[..6].to_string();
            assert!(callsign.len() == 6);
            //assert_eq!(callsign.chars().nth(2).unwrap().is_ascii_digit());
            return DenormalizedCompound(Compound::Callsign {
                base: callsign,
                is_portable: p,
            });
        }
        DenormalizedCompound(callsign)
    }

    /// Trick and adapt for some country
    /// Make sure it is at least 6 letters long including spaces if not long enough
    /// See pack_callsign_str
    fn normalize(DenormalizedCompound(callsign): DenormalizedCompound) -> Compound {
        let callsign = callsign.normalize();
        if let Compound::Callsign {
            ref base,
            is_portable: p,
        } = callsign
        {
            let callsign = base;
            if callsign.starts_with("3DA0") {
                // workaround for swaziland
                let callsign = "3DA0".to_string() + &callsign[4..];
                return Compound::Callsign { base: callsign, is_portable: p };
            } else if callsign.starts_with("Q") {
                // workaround for sguineawaziland
                let callsign = "3X".to_string() + &callsign[1..];
                return Compound::Callsign { base: callsign, is_portable: p };
            }
        }
        callsign
    }

    /// Pack a denormalized callsign,
    /// ie must match pattern (([0-9A-Z ])([0-9A-Z])([0-9])([A-Z ])([A-Z ])([A-Z ]))
    fn pack_callsign_str(callsign: &str) -> Option<Js8PackedCompound> {
        if !callsign.chars().nth(2)?.is_digit(10) {
            return None;
        }
        let mut indexes = callsign
            .chars()
            .enumerate()
            .map(|(i, c)| JS8Protocol::ALPHANUMERIC.find(c))
            .collect::<Vec<Option<usize>>>();
        if indexes.len() < 6 {
            return None;
        }
        indexes.resize(6, None);
        let n28 = indexes[0]? as u32;
        let n28 = n28 * 36 + (indexes[1]? as u32);
        let n28 = n28 * 10 + (indexes[2]? as u32);
        let n28 = n28 * 27 + (indexes[3]? as u32) - 10;
        let n28 = n28 * 27 + (indexes[4]? as u32) - 10;
        let n28 = n28 * 27 + (indexes[5]? as u32) - 10;
        Some(Js8PackedCompound::from(n28))
    }

    /// Pack a valid callsign into a 28-bits integer
    pub fn pack_callsign(callsign: Compound) -> Option<Js8PackedCompound> {
        let callsign = JS8Protocol::denormalize(callsign);
        if let DenormalizedCompound(Compound::BaseGroup { kind }) = callsign {
            return Some(Js8PackedCompound::from(kind as u32));
        }
        if let DenormalizedCompound(Compound::Callsign {
            base: callsign,
            is_portable: _,
        }) = callsign
        {
            return JS8Protocol::pack_callsign_str(&callsign);
        }
        None
    }

    pub fn unpack_callsign(callsign: Js8PackedCompound) -> Option<Compound> {
        let n28 = u32::from(callsign);
        if JS8Protocol::NBASECALL < n28 && n28 <= JS8Protocol::NBASECALL + BaseGroup::iter().len() as u32 {
            let callsign = BaseGroup::iter().nth((n28 - JS8Protocol::NBASECALL - 1) as usize)?;
            return Some(Compound::BaseGroup { kind: callsign });
        }
        let value = n28;
        let mut word = [' '; 6];

        let tmp = value % 27 + 10;
        word[5] = JS8Protocol::ALPHANUMERIC.chars().nth(tmp as usize)?;
        let value = value / 27;

        let tmp = value % 27 + 10;
        word[4] = JS8Protocol::ALPHANUMERIC.chars().nth(tmp as usize)?;
        let value = value / 27;

        let tmp = value % 27 + 10;
        word[3] = JS8Protocol::ALPHANUMERIC.chars().nth(tmp as usize)?;
        let value = value / 27;

        let tmp = value % 10;
        word[2] = JS8Protocol::ALPHANUMERIC.chars().nth(tmp as usize)?;
        let value = value / 10;

        let tmp = value % 36;
        word[1] = JS8Protocol::ALPHANUMERIC.chars().nth(tmp as usize)?;
        let value = value / 36;

        let tmp = value;
        word[0] = JS8Protocol::ALPHANUMERIC.chars().nth(tmp as usize)?;

        let word: String = word.iter().collect();
        let word = word.trim();
        // TODO js8 renormalize()
        Some(JS8Protocol::normalize(DenormalizedCompound(Compound::from_str(&word).ok()?)))
    }
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
pub fn pack75(msg: &str) -> Option<[u8; 10]> {
    let delimiters_index: Vec<_> = msg.match_indices(char::is_whitespace).collect();
    let first_space_index = delimiters_index[0].0 + 1;
    let second_space_index = delimiters_index.get(1);
    let n28a = pack28(&msg[..first_space_index])?;
    let n28b = pack28(&msg[first_space_index..])?;
    let i3: u8 = 1;
    let igrid4: u16;
    if let Some(s2) = second_space_index {
        igrid4 = pack_grid_or_report(&msg[s2.0 + 1..]).ok()?;
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
    result[9] = (igrid4 << 6) as u8 | (i3 << 3);
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

/// Pack a message into twelve 6-bits symbols + 3 bits for type + 12-bit CRC,
/// ie 87 bits
/// (6*12+3) + 12 = 75 + 12 = 87

pub fn pack87(msg: &str) -> Option<[u8; 11]> {
    let data = pack75(msg)?;
    let crc = crc12(&data);
    let mut result: [u8; 11] = [0; 11];
    result[..10].copy_from_slice(&data);
    // 75bits = 9*8 + 3
    // 12bits = 4 + 8
    // so to stick the 12 bits to the existing one,
    // requires 1-shift to the left.
    result[9] = result[9] | (crc >> 7) as u8;
    result[10] = (crc << 1 & 0x0FF) as u8;
    Some(result)
}

pub fn create_generator_matrix() -> [[u8; 87]; 87] {
    let raw_gen_matrix: [[u16; 11]; 87] = [
        [
            0x23, 0xbb, 0xa8, 0x30, 0xe2, 0x3b, 0x6b, 0x6f, 0x50, 0x98, 0x2e,
        ],
        [
            0x1f, 0x8e, 0x55, 0xda, 0x21, 0x8c, 0x5d, 0xf3, 0x30, 0x90, 0x52,
        ],
        [
            0xca, 0x7b, 0x32, 0x17, 0xcd, 0x92, 0xbd, 0x59, 0xa5, 0xae, 0x20,
        ],
        [
            0x56, 0xf7, 0x83, 0x13, 0x53, 0x7d, 0x0f, 0x43, 0x82, 0x96, 0x4e,
        ],
        [
            0x29, 0xc2, 0x9d, 0xba, 0x9c, 0x54, 0x5e, 0x26, 0x77, 0x62, 0xfe,
        ],
        [
            0x6b, 0xe3, 0x96, 0xb5, 0xe2, 0xe8, 0x19, 0xe3, 0x73, 0x34, 0x0c,
        ],
        [
            0x29, 0x35, 0x48, 0xa1, 0x38, 0x85, 0x83, 0x28, 0xaf, 0x42, 0x10,
        ],
        [
            0xcb, 0x6c, 0x6a, 0xfc, 0xdc, 0x28, 0xbb, 0x3f, 0x7c, 0x6e, 0x86,
        ],
        [
            0x3f, 0x2a, 0x86, 0xf5, 0xc5, 0xbd, 0x22, 0x5c, 0x96, 0x11, 0x50,
        ],
        [
            0x84, 0x9d, 0xd2, 0xd6, 0x36, 0x73, 0x48, 0x18, 0x60, 0xf6, 0x2c,
        ],
        [
            0x56, 0xcd, 0xae, 0xc6, 0xe7, 0xae, 0x14, 0xb4, 0x3f, 0xee, 0xee,
        ],
        [
            0x04, 0xef, 0x5c, 0xfa, 0x37, 0x66, 0xba, 0x77, 0x8f, 0x45, 0xa4,
        ],
        [
            0xc5, 0x25, 0xae, 0x4b, 0xd4, 0xf6, 0x27, 0x32, 0x0a, 0x39, 0x74,
        ],
        [
            0xfe, 0x37, 0x80, 0x29, 0x41, 0xd6, 0x6d, 0xde, 0x02, 0xb9, 0x9c,
        ],
        [
            0x41, 0xfd, 0x95, 0x20, 0xb2, 0xe4, 0xab, 0xeb, 0x2f, 0x98, 0x9c,
        ],
        [
            0x40, 0x90, 0x7b, 0x01, 0x28, 0x0f, 0x03, 0xc0, 0x32, 0x39, 0x46,
        ],
        [
            0x7f, 0xb3, 0x6c, 0x24, 0x08, 0x5a, 0x34, 0xd8, 0xc1, 0xdb, 0xc4,
        ],
        [
            0x40, 0xfc, 0x3e, 0x44, 0xbb, 0x7d, 0x2b, 0xb2, 0x75, 0x6e, 0x44,
        ],
        [
            0xd3, 0x8a, 0xb0, 0xa1, 0xd2, 0xe5, 0x2a, 0x8e, 0xc3, 0xbc, 0x76,
        ],
        [
            0x3d, 0x0f, 0x92, 0x9e, 0xf3, 0x94, 0x9b, 0xd8, 0x4d, 0x47, 0x34,
        ],
        [
            0x45, 0xd3, 0x81, 0x4f, 0x50, 0x40, 0x64, 0xf8, 0x05, 0x49, 0xae,
        ],
        [
            0xf1, 0x4d, 0xbf, 0x26, 0x38, 0x25, 0xd0, 0xbd, 0x04, 0xb0, 0x5e,
        ],
        [
            0xf0, 0x8a, 0x91, 0xfb, 0x2e, 0x1f, 0x78, 0x29, 0x06, 0x19, 0xa8,
        ],
        [
            0x7a, 0x8d, 0xec, 0x79, 0xa5, 0x1e, 0x8a, 0xc5, 0x38, 0x80, 0x22,
        ],
        [
            0xca, 0x41, 0x86, 0xdd, 0x44, 0xc3, 0x12, 0x15, 0x65, 0xcf, 0x5c,
        ],
        [
            0xdb, 0x71, 0x4f, 0x8f, 0x64, 0xe8, 0xac, 0x7a, 0xf1, 0xa7, 0x6e,
        ],
        [
            0x8d, 0x02, 0x74, 0xde, 0x71, 0xe7, 0xc1, 0xa8, 0x05, 0x5e, 0xb0,
        ],
        [
            0x51, 0xf8, 0x15, 0x73, 0xdd, 0x40, 0x49, 0xb0, 0x82, 0xde, 0x14,
        ],
        [
            0xd0, 0x37, 0xdb, 0x82, 0x51, 0x75, 0xd8, 0x51, 0xf3, 0xaf, 0x00,
        ],
        [
            0xd8, 0xf9, 0x37, 0xf3, 0x18, 0x22, 0xe5, 0x7c, 0x56, 0x23, 0x70,
        ],
        [
            0x1b, 0xf1, 0x49, 0x06, 0x07, 0xc5, 0x40, 0x32, 0x66, 0x0e, 0xde,
        ],
        [
            0x16, 0x16, 0xd7, 0x80, 0x18, 0xd0, 0xb4, 0x74, 0x5c, 0xa0, 0xf2,
        ],
        [
            0xa9, 0xfa, 0x8e, 0x50, 0xbc, 0xb0, 0x32, 0xc8, 0x5e, 0x33, 0x04,
        ],
        [
            0x83, 0xf6, 0x40, 0xf1, 0xa4, 0x8a, 0x8e, 0xbc, 0x04, 0x43, 0xea,
        ],
        [
            0xec, 0xa9, 0xaf, 0xa0, 0xf6, 0xb0, 0x1d, 0x92, 0x30, 0x5e, 0xdc,
        ],
        [
            0x37, 0x76, 0xaf, 0x54, 0xcc, 0xfb, 0xae, 0x91, 0x6a, 0xfd, 0xe6,
        ],
        [
            0x6a, 0xbb, 0x21, 0x2d, 0x97, 0x39, 0xdf, 0xc0, 0x25, 0x80, 0xf2,
        ],
        [
            0x05, 0x20, 0x9a, 0x0a, 0xbb, 0x53, 0x0b, 0x9e, 0x7e, 0x34, 0xb0,
        ],
        [
            0x61, 0x2f, 0x63, 0xac, 0xc0, 0x25, 0xb6, 0xab, 0x47, 0x6f, 0x7c,
        ],
        [
            0x0a, 0xf7, 0x72, 0x31, 0x61, 0xec, 0x22, 0x30, 0x80, 0xbe, 0x86,
        ],
        [
            0xa8, 0xfc, 0x90, 0x69, 0x76, 0xc3, 0x56, 0x69, 0xe7, 0x9c, 0xe0,
        ],
        [
            0x45, 0xb7, 0xab, 0x62, 0x42, 0xb7, 0x74, 0x74, 0xd9, 0xf1, 0x1a,
        ],
        [
            0xb2, 0x74, 0xdb, 0x8a, 0xbd, 0x3c, 0x6f, 0x39, 0x6e, 0xa3, 0x56,
        ],
        [
            0x90, 0x59, 0xdf, 0xa2, 0xbb, 0x20, 0xef, 0x7e, 0xf7, 0x3a, 0xd4,
        ],
        [
            0x3d, 0x18, 0x8e, 0xa4, 0x77, 0xf6, 0xfa, 0x41, 0x31, 0x7a, 0x4e,
        ],
        [
            0x8d, 0x90, 0x71, 0xb7, 0xe7, 0xa6, 0xa2, 0xee, 0xd6, 0x96, 0x5e,
        ],
        [
            0xa3, 0x77, 0x25, 0x37, 0x73, 0xea, 0x67, 0x83, 0x67, 0xc3, 0xf6,
        ],
        [
            0xec, 0xbd, 0x7c, 0x73, 0xb9, 0xcd, 0x34, 0xc3, 0x72, 0x0c, 0x8a,
        ],
        [
            0xb6, 0x53, 0x7f, 0x41, 0x7e, 0x61, 0xd1, 0xa7, 0x08, 0x53, 0x36,
        ],
        [
            0x6c, 0x28, 0x0d, 0x2a, 0x05, 0x23, 0xd9, 0xc4, 0xbc, 0x59, 0x46,
        ],
        [
            0xd3, 0x6d, 0x66, 0x2a, 0x69, 0xae, 0x24, 0xb7, 0x4d, 0xcb, 0xd8,
        ],
        [
            0xd7, 0x47, 0xbf, 0xc5, 0xfd, 0x65, 0xef, 0x70, 0xfb, 0xd9, 0xbc,
        ],
        [
            0xa9, 0xfa, 0x2e, 0xef, 0xa6, 0xf8, 0x79, 0x6a, 0x35, 0x57, 0x72,
        ],
        [
            0xcc, 0x9d, 0xa5, 0x5f, 0xe0, 0x46, 0xd0, 0xcb, 0x3a, 0x77, 0x0c,
        ],
        [
            0xf6, 0xad, 0x48, 0x24, 0xb8, 0x7c, 0x80, 0xeb, 0xfc, 0xe4, 0x66,
        ],
        [
            0xcc, 0x6d, 0xe5, 0x97, 0x55, 0x42, 0x09, 0x25, 0xf9, 0x0e, 0xd2,
        ],
        [
            0x16, 0x4c, 0xc8, 0x61, 0xbd, 0xd8, 0x03, 0xc5, 0x47, 0xf2, 0xac,
        ],
        [
            0xc0, 0xfc, 0x3e, 0xc4, 0xfb, 0x7d, 0x2b, 0xb2, 0x75, 0x66, 0x44,
        ],
        [
            0x0d, 0xbd, 0x81, 0x6f, 0xba, 0x15, 0x43, 0xf7, 0x21, 0xdc, 0x72,
        ],
        [
            0xa0, 0xc0, 0x03, 0x3a, 0x52, 0xab, 0x62, 0x99, 0x80, 0x2f, 0xd2,
        ],
        [
            0xbf, 0x4f, 0x56, 0xe0, 0x73, 0x27, 0x1f, 0x6a, 0xb4, 0xbf, 0x80,
        ],
        [
            0x57, 0xda, 0x6d, 0x13, 0xcb, 0x96, 0xa7, 0x68, 0x9b, 0x27, 0x90,
        ],
        [
            0x81, 0xcf, 0xc6, 0xf1, 0x8c, 0x35, 0xb1, 0xe1, 0xf1, 0x71, 0x14,
        ],
        [
            0x48, 0x1a, 0x2a, 0x0d, 0xf8, 0xa2, 0x35, 0x83, 0xf8, 0x2d, 0x6c,
        ],
        [
            0x1a, 0xc4, 0x67, 0x2b, 0x54, 0x9c, 0xd6, 0xdb, 0xa7, 0x9b, 0xcc,
        ],
        [
            0xc8, 0x7a, 0xf9, 0xa5, 0xd5, 0x20, 0x6a, 0xbc, 0xa5, 0x32, 0xa8,
        ],
        [
            0x97, 0xd4, 0x16, 0x9c, 0xb3, 0x3e, 0x74, 0x35, 0x71, 0x8d, 0x90,
        ],
        [
            0xa6, 0x57, 0x3f, 0x3d, 0xc8, 0xb1, 0x6c, 0x9d, 0x19, 0xf7, 0x46,
        ],
        [
            0x2c, 0x41, 0x42, 0xbf, 0x42, 0xb0, 0x1e, 0x71, 0x07, 0x6a, 0xcc,
        ],
        [
            0x08, 0x1c, 0x29, 0xa1, 0x0d, 0x46, 0x8c, 0xcd, 0xbc, 0xec, 0xb6,
        ],
        [
            0x5b, 0x0f, 0x77, 0x42, 0xbc, 0xa8, 0x6b, 0x80, 0x12, 0x60, 0x9a,
        ],
        [
            0x01, 0x2d, 0xee, 0x21, 0x98, 0xeb, 0xa8, 0x2b, 0x19, 0xa1, 0xda,
        ],
        [
            0xf1, 0x62, 0x77, 0x01, 0xa2, 0xd6, 0x92, 0xfd, 0x94, 0x49, 0xe6,
        ],
        [
            0x35, 0xad, 0x3f, 0xb0, 0xfa, 0xeb, 0x5f, 0x1b, 0x0c, 0x30, 0xdc,
        ],
        [
            0xb1, 0xca, 0x4e, 0xa2, 0xe3, 0xd1, 0x73, 0xba, 0xd4, 0x37, 0x9c,
        ],
        [
            0x37, 0xd8, 0xe0, 0xaf, 0x92, 0x58, 0xb9, 0xe8, 0xc5, 0xf9, 0xb2,
        ],
        [
            0xcd, 0x92, 0x1f, 0xdf, 0x59, 0xe8, 0x82, 0x68, 0x37, 0x63, 0xf6,
        ],
        [
            0x61, 0x14, 0xe0, 0x84, 0x83, 0x04, 0x3f, 0xd3, 0xf3, 0x8a, 0x8a,
        ],
        [
            0x2e, 0x54, 0x7d, 0xd7, 0xa0, 0x5f, 0x65, 0x97, 0xaa, 0xc5, 0x16,
        ],
        [
            0x95, 0xe4, 0x5e, 0xcd, 0x01, 0x35, 0xac, 0xa9, 0xd6, 0xe6, 0xae,
        ],
        [
            0xb3, 0x3e, 0xc9, 0x7b, 0xe8, 0x3c, 0xe4, 0x13, 0xf9, 0xac, 0xc8,
        ],
        [
            0xc8, 0xb5, 0xdf, 0xfc, 0x33, 0x50, 0x95, 0xdc, 0xdc, 0xaf, 0x2a,
        ],
        [
            0x3d, 0xd0, 0x1a, 0x59, 0xd8, 0x63, 0x10, 0x74, 0x3e, 0xc7, 0x52,
        ],
        [
            0x14, 0xcd, 0x0f, 0x64, 0x2f, 0xc0, 0xc5, 0xfe, 0x3a, 0x65, 0xca,
        ],
        [
            0x3a, 0x0a, 0x1d, 0xfd, 0x7e, 0xee, 0x29, 0xc2, 0xe8, 0x27, 0xe0,
        ],
        [
            0x8a, 0xbd, 0xb8, 0x89, 0xef, 0xbe, 0x39, 0xa5, 0x10, 0xa1, 0x18,
        ],
        [
            0x3f, 0x23, 0x1f, 0x21, 0x20, 0x55, 0x37, 0x1c, 0xf3, 0xe2, 0xa2,
        ],
    ];
    let mut gen = [[0 as u8; 87]; 87];
    for i in 0..87 {
        for j in 1..11 {
            let istr = raw_gen_matrix[i][j - 1];
            for jj in 1..8 {
                let icol = (j - 1) * 8 + jj;
                if icol < 87 {
                    if 1 == ((istr >> (8 - jj)) & 0x1) {
                        gen[i][icol] = 1;
                    }
                }
            }
        }
    }
    assert!(gen.len() == 87);
    gen
}

pub fn encode174(message: [u8; 10]) -> Option<[u8; 11]> {
    const N: usize = 174;
    const M: usize = 87;
    const K: usize = N - M;
    let gen = create_generator_matrix();
    let colorder: [usize; 174] = [
        0, 1, 2, 3, 30, 4, 5, 6, 7, 8, 9, 10, 11, 32, 12, 40, 13, 14, 15, 16, 17, 18, 37, 45, 29,
        19, 20, 21, 41, 22, 42, 31, 33, 34, 44, 35, 47, 51, 50, 43, 36, 52, 63, 46, 25, 55, 27, 24,
        23, 53, 39, 49, 59, 38, 48, 61, 60, 57, 28, 62, 56, 58, 65, 66, 26, 70, 64, 69, 68, 67, 74,
        71, 54, 76, 72, 75, 78, 77, 80, 79, 73, 83, 84, 81, 82, 85, 86, 87, 88, 89, 90, 91, 92, 93,
        94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
        113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130,
        131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148,
        149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166,
        167, 168, 169, 170, 171, 172, 173,
    ];
    //   let mut pchecks = [0 as u8; K];
    //   for i in 1..M {
    //     let mut nsum: u32 =0;
    //     for j in 1..K {
    //         nsum=nsum+message[j]*gen[i][j];
    //     }
    //     pchecks[i]= nsum % 2;
    //      }
    // itmp(1:M)=pchecks
    // itmp(M+1:N)=message(1:K)
    // codeword(colorder+1)=itmp(1:N)
    None
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    extern crate quickcheck;

    use std::str::FromStr;

    use crate::compound::BaseGroup;
    use crate::compound::Compound;
    use crate::pack::JS8Protocol;
    use crate::pack::Js8PackedCompound;
    use crate::pack::{char_index, fmtmsg, grid2deg, GridError};
    use crate::pack::{crc12, pack28, pack75, pack87, pack_grid_or_report};
    use crate::pack::{NBASE, NGBASE};
    use quickcheck::TestResult;
    use regex::Regex;

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
    fn qc_pack28_cq_freq(freq: u16) -> TestResult {
        if freq > 999 {
            return TestResult::discard();
        }
        let s = format!("CQ {:03}", freq);
        TestResult::from_bool(pack28(&s) == Some(NBASE + 3 + (freq as u32)))
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
    fn qc_packgrid_r_qc(n: u8) -> TestResult {
        if n < 1 || n > 30 {
            return TestResult::discard();
        }
        let grid = format!("R-{:02}", n);
        TestResult::from_bool(pack_grid_or_report(&grid) == Ok(NGBASE + 31 + n as u16))
    }

    #[quickcheck]
    fn qc_packgrid_signal_report_qc(n: u8) -> TestResult {
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
            pack75("CQ DL1ABC JO62"),
            Some([250, 8, 49, 147, 68, 74, 17, 142, 209, 8])
        );
    }

    #[test]
    fn crc_test() {
        let data = pack75("CQ DL1ABC JO62").unwrap();
        let crc = crc12(&data);
        assert_eq!(crc, 2688);
    }

    #[test]
    fn pack87_test() {
        assert_eq!(
            pack87("CQ DL1ABC JO62"),
            Some([
                250,
                8,
                49,
                147,
                68,
                74,
                17,
                142,
                209,
                8 | (((2688 & 0xFFF) << 1) >> 8) as u8,
                ((2688 & 0x0FF) as u8) << 1
            ])
        );
    }

    #[test]
    fn pack_callsign_known() {
        let ll3ajg_packed = Some(Js8PackedCompound::from(152996208u32));
        assert_eq!(
            JS8Protocol::pack_callsign(Compound::Callsign {
                base: "LL3AJG".into(),
                is_portable: false
            }),
            ll3ajg_packed
        );
        let v = JS8Protocol::pack_callsign(Compound::Callsign {
            base: "L0ABC".into(),
            is_portable: false,
        });
        assert_eq!(u32::from(v.unwrap()), 259225139u32);
        assert_eq!(
            JS8Protocol::pack_callsign(Compound::Callsign {
                base: "LL3JG".into(),
                is_portable: false
            }),
            Some(Js8PackedCompound::from(153002708u32))
        );
        assert_eq!(
            Compound::from_str("LL3AJG").ok(),
            JS8Protocol::unpack_callsign(ll3ajg_packed.unwrap())
        );
    }

    #[test]
    fn pack_basegroup() {
        let js8_in_order_kinds = [
            BaseGroup::Incomplete,
            BaseGroup::AllCall,
            BaseGroup::JS8Net,
            BaseGroup::DX_NA,
            BaseGroup::DX_SA,
        ];
        for (i, k) in js8_in_order_kinds.iter().enumerate() {
            let callsign = Compound::BaseGroup { kind: *k };
            assert_eq!(
                JS8Protocol::pack_callsign(callsign.clone()),
                Some(Js8PackedCompound::from(
                    JS8Protocol::NBASECALL + 1 + (i as u32)
                ))
            );
            assert_eq!(
                Some(callsign),
                JS8Protocol::unpack_callsign(Js8PackedCompound::from(JS8Protocol::NBASECALL + 1 + (i as u32)))
            );
        }
    }

    #[quickcheck]
    fn qc_pack_swaziland_callsign(c1: char, c2: char, c3: char) -> TestResult {
        if !c1.is_alphabetic() || !c2.is_alphabetic() || !c3.is_alphabetic() {
            return TestResult::discard();
        }
        let callsign = format!("3DA0{}{}{}", c1, c2, c3);
        if !callsign.is_ascii() {
            return TestResult::discard();
        }
        let same_as = format!("3D0{}{}{}", c1, c2, c3);
        let callsign = Compound::Callsign {
            base: callsign,
            is_portable: false,
        };
        let actual = JS8Protocol::pack_callsign(callsign);
        let same_as = Compound::Callsign {
            base: same_as,
            is_portable: false,
        };
        let expected = JS8Protocol::pack_callsign(same_as);
        TestResult::from_bool(expected == actual)
    }

    #[test]
    fn pack_guinea_callsign() {
        let callsign = Compound::from_str("3XA0XYZ").unwrap();
        let packed_callsign = JS8Protocol::pack_callsign(callsign.clone());
        let denormalized_callsign = Compound::from_str("QA0XYZ").unwrap();
        let packed_denormalized_callsign = JS8Protocol::pack_callsign(denormalized_callsign);
        assert_eq!(
            packed_callsign,
            packed_denormalized_callsign
        );
        assert_eq!(
            Some(callsign),
            JS8Protocol::unpack_callsign(packed_denormalized_callsign.unwrap())
        );
    }

    #[quickcheck]
    fn qc_pack_unpack_callsig(c1: char, c2: char, c3: char, c4: char, c5: char, c6: char) -> TestResult {
        let callsign: String = [c1, c2, c3, c4, c5, c6].iter().collect();
        let regex = Regex::new(r"(([0-9A-Z ])([0-9A-Z])([0-9])([A-Z ])([A-Z ])([A-Z ]))").unwrap();
        if !regex.is_match(&callsign) {
            return TestResult::discard();
        }
        let callsign = Compound::from_str(callsign.trim()).unwrap();
        let packed_callsign = JS8Protocol::pack_callsign(callsign.clone()).unwrap();
        let unpacked_packed_callsign = JS8Protocol::unpack_callsign(packed_callsign).unwrap();
        if callsign != unpacked_packed_callsign {
            return TestResult::failed();
        }
        TestResult::passed()
    }

    #[quickcheck]
    fn qc_unpack_pack_callsign(n28: u32) {
        let n28 = Js8PackedCompound::from(n28);
        let unpacked_callsign = JS8Protocol::unpack_callsign(n28);
        if unpacked_callsign == None {
            return
        }
        let unpacked_callsign = unpacked_callsign.unwrap();
        if let Compound::Callsign {ref base, is_portable: _ } = unpacked_callsign {
            let regex = Regex::new(r"(([0-9A-Z ])([0-9A-Z])([0-9])([A-Z ])([A-Z ])([A-Z ]))").unwrap();
            if !regex.is_match(base) {
                return;
            }
        }
        let packed_unpacked_callsign = JS8Protocol::pack_callsign(unpacked_callsign);
        if packed_unpacked_callsign == None {
            return
        }
        assert_eq!(
            Some(n28),
            packed_unpacked_callsign
        );
    }

//     #[test]
//     fn unpack_pack_callsign() {
//         let n28 = 0b00001111100001000010011010010111u32;
//         let n28 = Js8PackedCompound::from(n28);
//         let unpacked_callsign = JS8Protocol::unpack_callsign(n28).unwrap();
//         if let Compound::Callsign {ref base, is_portable: _ } = unpacked_callsign {
//             let regex = Regex::new(r"(([0-9A-Z ])([0-9A-Z])([0-9])([A-Z ])([A-Z ])([A-Z ]))").unwrap();
//             if !regex.is_match(base) {
//                 return;
//             }
//         }
//         let packed_unpacked_callsign = JS8Protocol::pack_callsign(unpacked_callsign).unwrap();
//         assert_eq!(
//             u32::from(n28),
//             u32::from(packed_unpacked_callsign)
//         );
//     }

    // #[quickcheck]
    // fn qc_unpack_pack_compound(callsign: Compound) {
    //     let n28 = JS8Protocol::pack_callsign(callsign.clone()).unwrap();
    //     let p = JS8Protocol::unpack_callsign(n28).unwrap();
    //     assert_eq!(
    //         callsign,
    //         p
    //     );
    // }
}
