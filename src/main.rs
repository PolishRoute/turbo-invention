#![allow(dead_code)]
#![allow(unused_imports)]
mod parse;

use std::io::{Read, SeekFrom, Seek};
use std::ops::Range;
use std::collections::HashMap;
use std::convert::TryInto;
use crate::parse::{read_bytecode, Parser};

type MyError = Box<dyn std::error::Error>;

struct Archive {
    scenarios: HashMap<i32, Range<usize>>,
}

struct Header {
    z_minus_one: i32,
    z_minus_two: i32,
    savepoint_message: i32,
    savepoint_selcom: i32,
    savepoint_seentop: i32,
    dramatic_personae: Vec<String>,
    use_xor2: bool,
}

struct Scenario {
    header: Header,

}

fn read_i32(data: &[u8]) -> i32 {
    i32::from_le_bytes(data[..4].try_into().unwrap())
}

fn read_i16(data: &[u8]) -> i16 {
    i16::from_le_bytes(data[..2].try_into().unwrap())
}

struct XorKey {
    xor: [u8; 16],
    offset: usize,
    length: usize,
}

const XOR_MASK: [u8; 256] = [
    0x8b, 0xe5, 0x5d, 0xc3, 0xa1, 0xe0, 0x30, 0x44, 0x00, 0x85, 0xc0, 0x74,
    0x09, 0x5f, 0x5e, 0x33, 0xc0, 0x5b, 0x8b, 0xe5, 0x5d, 0xc3, 0x8b, 0x45,
    0x0c, 0x85, 0xc0, 0x75, 0x14, 0x8b, 0x55, 0xec, 0x83, 0xc2, 0x20, 0x52,
    0x6a, 0x00, 0xe8, 0xf5, 0x28, 0x01, 0x00, 0x83, 0xc4, 0x08, 0x89, 0x45,
    0x0c, 0x8b, 0x45, 0xe4, 0x6a, 0x00, 0x6a, 0x00, 0x50, 0x53, 0xff, 0x15,
    0x34, 0xb1, 0x43, 0x00, 0x8b, 0x45, 0x10, 0x85, 0xc0, 0x74, 0x05, 0x8b,
    0x4d, 0xec, 0x89, 0x08, 0x8a, 0x45, 0xf0, 0x84, 0xc0, 0x75, 0x78, 0xa1,
    0xe0, 0x30, 0x44, 0x00, 0x8b, 0x7d, 0xe8, 0x8b, 0x75, 0x0c, 0x85, 0xc0,
    0x75, 0x44, 0x8b, 0x1d, 0xd0, 0xb0, 0x43, 0x00, 0x85, 0xff, 0x76, 0x37,
    0x81, 0xff, 0x00, 0x00, 0x04, 0x00, 0x6a, 0x00, 0x76, 0x43, 0x8b, 0x45,
    0xf8, 0x8d, 0x55, 0xfc, 0x52, 0x68, 0x00, 0x00, 0x04, 0x00, 0x56, 0x50,
    0xff, 0x15, 0x2c, 0xb1, 0x43, 0x00, 0x6a, 0x05, 0xff, 0xd3, 0xa1, 0xe0,
    0x30, 0x44, 0x00, 0x81, 0xef, 0x00, 0x00, 0x04, 0x00, 0x81, 0xc6, 0x00,
    0x00, 0x04, 0x00, 0x85, 0xc0, 0x74, 0xc5, 0x8b, 0x5d, 0xf8, 0x53, 0xe8,
    0xf4, 0xfb, 0xff, 0xff, 0x8b, 0x45, 0x0c, 0x83, 0xc4, 0x04, 0x5f, 0x5e,
    0x5b, 0x8b, 0xe5, 0x5d, 0xc3, 0x8b, 0x55, 0xf8, 0x8d, 0x4d, 0xfc, 0x51,
    0x57, 0x56, 0x52, 0xff, 0x15, 0x2c, 0xb1, 0x43, 0x00, 0xeb, 0xd8, 0x8b,
    0x45, 0xe8, 0x83, 0xc0, 0x20, 0x50, 0x6a, 0x00, 0xe8, 0x47, 0x28, 0x01,
    0x00, 0x8b, 0x7d, 0xe8, 0x89, 0x45, 0xf4, 0x8b, 0xf0, 0xa1, 0xe0, 0x30,
    0x44, 0x00, 0x83, 0xc4, 0x08, 0x85, 0xc0, 0x75, 0x56, 0x8b, 0x1d, 0xd0,
    0xb0, 0x43, 0x00, 0x85, 0xff, 0x76, 0x49, 0x81, 0xff, 0x00, 0x00, 0x04,
    0x00, 0x6a, 0x00, 0x76
];

const LITTLE_BUSTERS_XOR_MASK: [XorKey; 1] = [
    XorKey {
        xor: [
            0xa8, 0x28, 0xfd, 0x66,
            0xa0, 0x23, 0x77, 0x69,
            0xf9, 0x45, 0xf8, 0x2c,
            0x7c, 0x00, 0xad, 0xf4
        ],
        offset: 256,
        length: 257,
    }];

const LITTLE_BUSTERS_EX_XOR_MASK: [XorKey; 4] = [
    XorKey {
        xor: [0xa8, 0x28, 0xfd, 0x71,
            0xb4, 0x23, 0x64, 0x15,
            0x96, 0x48, 0x8a, 0x43,
            0x62, 0x0e, 0xad, 0xf0],
        offset: 256,
        length: 128,
    },
    XorKey {
        xor: [
            0xde, 0xd9, 0x4a, 0x18,
            0xaf, 0x23, 0x1d, 0x9a,
            0xac, 0x23, 0x25, 0x48,
            0xd8, 0xd4, 0x8f, 0xa7
        ],
        offset: 384,
        length: 128,
    },
    XorKey {
        xor: [0xde, 0xf1, 0xb7, 0x69, 0x1b, 0x00, 0x79, 0x8f, 0x3a, 0x6b, 0xaf, 0x0b,
            0xba, 0xda, 0x22, 0x57],
        offset: 512,
        length: 16,
    },
    XorKey {
        xor: [0x76, 0xf1, 0xb7, 0x69, 0x1b, 0x00, 0x79, 0x8f, 0x3a, 0x6b, 0xaf, 0x0b,
            0xba, 0xda, 0x22, 0x57],
        offset: 528,
        length: 113,
    }
];

const CLANNAD_FULL_VOICE_XOR_MASK: [XorKey; 1] = [
    XorKey {
        xor: [0xAF, 0x2F, 0xFB, 0x6B, 0xAF, 0x30, 0x77, 0x17, 0x87, 0x48, 0xFE, 0x2C,
            0x68, 0x1A, 0xB9, 0xF0],
        offset: 256,
        length: 257,
    }
];

const SNOW_STANDARD_EDITION_XOR_MASK: [XorKey; 1] = [
    XorKey {
        xor: [0xe4, 0xab, 0xa2, 0xc9, 0xec, 0x39, 0x36, 0x62, 0xc9, 0x03, 0xba, 0x6d,
            0x2e, 0x9c, 0xf2, 0x64],
        offset: 256,
        length: 257,
    }
];

const KUD_WAFTER_XOR_MASK: [XorKey; 1] = [
    XorKey {
        xor: [0x67, 0x1c, 0x21, 0xbe, 0x6f, 0xef, 0xb5, 0x16, 0x4a, 0x82, 0x39, 0x2b,
            0xad, 0x3a, 0x71, 0x3f],
        offset: 256,
        length: 257,
    }
];

const KUD_WAFTER_ALL_AGES_XOR_MASK: [XorKey; 1] = [
    XorKey {
        xor: [0xaf, 0x3f, 0xe6, 0x63, 0xad, 0x3a, 0x69, 0x18, 0x85, 0x45, 0xe5, 0x40,
            0x1e, 0x7e, 0xb9, 0xe0],
        offset: 256,
        length: 257,
    }
];

struct ConstructionData {
    kidoku_table: Vec<usize>,
    offsets: HashMap<usize, usize>,
    null: Option<usize>,
}

fn read_script(data: &[u8], use_xor_2: bool, second_level_xor_key: Option<&[XorKey]>) -> (ConstructionData, Vec<u8>) {
    // Kidoku/entrypoint table
    let kidoku_offs = read_i32(&data[0x08..]) as usize;
    let kidoku_len = read_i32(&data[0x0c..]) as usize;
    let mut kidoku_table = Vec::with_capacity(kidoku_len);
    for i in 0..kidoku_len {
        kidoku_table.push(read_i32(&data[kidoku_offs + i * 4..]) as usize);
    }

    // Decompress data
    let dlen = read_i32(&data[0x24..]) as usize;

    let mut keys = None;
    if use_xor_2 {
        if second_level_xor_key.is_some() {
            keys = second_level_xor_key;
        } else {
            // Probably safe to assume that any game we don't know about has a
            // Japanese encoding.
            unimplemented!();
        }
    }


    let mut uncompressed = vec![0u8; dlen];
    let offset = read_i32(&data[0x20..]) as usize;
    let length = read_i32(&data[0x28..]) as usize;
    decompress(&data[offset..][..length], &mut uncompressed, keys);

    let cd = ConstructionData {
        kidoku_table,
        offsets: HashMap::new(),
        null: None,
    };

    (cd, uncompressed)
}


fn detect_key(regname: &[u8]) -> Option<&[XorKey]> {
    match regname {
        b"KEY\\CLANNAD_FV" => {
            Some(&CLANNAD_FULL_VOICE_XOR_MASK)
        }
        b"\x4b\x45\x59\x5c\x83\x8a\x83\x67\x83\x8b\x83\x6f\x83\x58\x83\x5e\x81\x5b\x83\x59\x81\x49" => {
            Some(&LITTLE_BUSTERS_XOR_MASK)
        }
        b"\x4b\x45\x59\x5c\x83\x8a\x83\x67\x83\x8b\x83\x6f\x83\x58\x83\x5e\x81\x5b\x83\x59\x81\x49\x82\x64\x82\x77" => {
            // "KEY\<little busters in katakana>!EX", with all fullwidth latin
            // characters.
            Some(&LITTLE_BUSTERS_EX_XOR_MASK)
        }
        b"StudioMebius\\SNOWSE" => {
            Some(&SNOW_STANDARD_EDITION_XOR_MASK)
        }
        b"\x4b\x45\x59\x5c\x83\x4e\x83\x68\x82\xed\x82\xd3\x82\xbd\x81\x5b" => {
            // "KEY\<Kud Wafter in hiragana>"
            Some(&KUD_WAFTER_XOR_MASK)
        }
        b"\x4b\x45\x59\x5c\x83\x4e\x83\x68\x82\xed\x82\xd3\x82\xbd\x81\x5b\x81\x79\x91\x53\x94\x4e\x97\xee\x91\xce\x8f\xdb\x94\xc5\x81\x7a" => {
            Some(&KUD_WAFTER_ALL_AGES_XOR_MASK)
        }
        _ => None,
    }
}

fn decompress(src: &[u8], dst: &mut [u8], per_game_xor_keys: Option<&[XorKey]>) {
    let mut src = src.iter()
        .copied()
        .zip(XOR_MASK.iter().copied().cycle())
        .map(|(byte, key)| byte ^ key)
        .skip(8);

    let mut d = 0;
    while let Some(mut flags) = src.next() {
        for _ in 0..8 {
            let copy = (flags & 1) == 1;
            if copy {
                dst[d] = src.next().unwrap();
                d += 1;
            } else {
                let count = match (src.next(), src.next()) {
                    (Some(b1), Some(b2)) => u16::from_le_bytes([b1, b2]),
                    (None, None) => break,
                    _ => unreachable!(),
                };
                let repeat = (count >> 4) as usize;
                let count = ((count & 0x0f) + 2) as usize;
                let start = d - repeat;
                dst.copy_within(start..start + count, d);
                d += count;
            }
            flags >>= 1;
        }
    }

    assert_eq!(d, dst.len(), "output buffer was not filled entirely");

    if let Some(keys) = per_game_xor_keys {
        for key in keys {
            for i in 0..key.length {
                dst[key.offset + i] ^= key.xor[i % 16]
            }
        }
    }
}

fn parse_header(data: &[u8]) -> Result<Header, MyError> {
    assert!(data.len() >= 0x1d0);
    let compiler = read_i32(&data);
    if compiler != 0x1d0 {
        unimplemented!("unsupported bytecode version");
    }

    let use_xor2 = match read_i32(&data[4..]) {
        10002 => false,
        110002 => true,
        1110002 => true,
        ver => unimplemented!("Unsupported compiler version {}", ver),
    };

    // Debug entrypoints
    let z_minus_one = read_i32(&data[0x2c..]);
    let z_minus_two = read_i32(&data[0x30..]);

    // Misc settinfs
    let savepoint_message = read_i32(&data[0x1c4..]);
    let savepoint_selcom = read_i32(&data[0x1c8..]);
    let savepoint_seentop = read_i32(&data[0x1cc..]);

    // Dramatic personae
    let len = read_i32(&data[0x18..]) as usize;
    let mut dramatic_personae = Vec::with_capacity(len);
    let mut offs = read_i32(&data[0x14..]) as usize;
    for _ in 0..len {
        let elen = read_i32(&data[offs..]);
        let elen = elen as usize;
        let name = &data[offs + 4..][..elen - 1];
        let name = encoding_rs::SHIFT_JIS.decode(name).0;
        dramatic_personae.push(name.into_owned());
        offs += elen + 4;
    }

    // TODO: rldev metadata

    Ok(Header {
        use_xor2,
        z_minus_one,
        z_minus_two,
        savepoint_message,
        savepoint_selcom,
        savepoint_seentop,
        dramatic_personae,
    })
}

fn main() -> Result<(), MyError> {
    let path = std::env::args_os().nth(1).unwrap_or_else(|| {
        r"C:\Users\Host\Downloads\SEEN.txt".into()
    });

    let file = std::fs::File::open(path)?;
    let mut reader = std::io::BufReader::new(file);
    let mut buff = vec![];
    reader.read_to_end(&mut buff)?;
    reader.seek(SeekFrom::Start(0))?;

    let mut total = std::time::Duration::default();
    let mut total_items = 0;

    let mut scenarios = Vec::new();
    for i in 0..10000 {
        let offs = {
            let mut buf = [0u8; 4];
            reader.read_exact(&mut buf)?;
            i32::from_le_bytes(buf)
        } as usize;

        if offs == 0 {
            continue;
        }

        let len = {
            let mut buf = [0u8; 4];
            reader.read_exact(&mut buf)?;
            i32::from_le_bytes(buf)
        } as usize;

        let range = offs..offs + len;
        scenarios.push((i, range.clone()));

        // if i < 840 {
        //     continue;
        // }

        let result = std::panic::catch_unwind(|| {
            println!("parsing scenario: {}", i);
            let f = &buff[range.clone()];
            let header = parse_header(&f).unwrap();
            let (cd, uncompressed) = read_script(&f, header.use_xor2, Some(&CLANNAD_FULL_VOICE_XOR_MASK));
            std::fs::write(format!("scenario{:04}.txt", i), &uncompressed).unwrap();

            let mut p = Parser::new_at(&uncompressed, 0);
            let s = std::time::Instant::now();
            let items = read_bytecode(&mut p, &cd.kidoku_table);
            (s.elapsed(), items)
        });
        match result {
            Ok((time, items)) => {
                total += time;
                total_items += items.len();
            }
            Err(e) => {
                println!("{:?}", e);
                panic!();
            }
        }
    }

    dbg!(total, total_items);

    Ok(())
}
