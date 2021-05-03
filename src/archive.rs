use std::convert::TryInto;
use std::io;
use std::io::Read;
use std::ops::Range;

use crate::keys::{detect_key, XorKey};
use crate::MyError;
use crate::parse::{Element, Parser, read_bytecode};

pub(crate) struct Archive<'d> {
    data: &'d [u8],
    scenarios: Vec<(u32, Range<usize>)>,
}

impl<'d> Archive<'d> {
    pub(crate) fn scenarios(&self) -> impl Iterator<Item=Scenario> + '_ {
        (0..self.scenarios.len()).map(move |idx| self.get(idx).unwrap())
    }

    pub(crate) fn get(&self, idx: usize) -> Option<Scenario> {
        Some(Scenario {
            range: self.scenarios[idx].1.clone(),
            data: self.data
        })
    }

    #[allow(unused)]
    pub(crate) fn get_by_id(&self, id: u32) -> Option<Scenario> {
        let idx = self.scenarios.binary_search_by_key(&id, |item| item.0).ok()?;
        self.get(idx)
    }
}

#[allow(unused)]
struct Header {
    z_minus_one: i32,
    z_minus_two: i32,
    savepoint_message: i32,
    savepoint_selcom: i32,
    savepoint_seentop: i32,
    dramatic_personae: Vec<String>,
    use_xor2: bool,
}

pub(crate) struct Scenario<'d> {
    range: Range<usize>,
    data: &'d [u8],
}

impl<'d> Scenario<'d> {
    pub(crate) fn read(&self) -> Option<Vec<(usize, Element)>>{
        let data = &self.data[self.range.clone()];
        let header = parse_header(&data).unwrap();
        let (cd, uncompressed) = read_script(
            &data,
            header.use_xor2,
            detect_key(b"KEY\\CLANNAD_FV"),
        );
        let mut parser = Parser::new_at(&uncompressed, 0);
        Some(read_bytecode(&mut parser, &cd.kidoku_table))
    }
}


pub(crate) fn parse_seen(data: &[u8]) -> io::Result<Archive> {
    let mut scenarios = Vec::new();
    let mut reader = io::Cursor::new(data);
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
    }
    Ok(Archive { scenarios, data })
}


fn read_i32(data: &[u8]) -> i32 {
    i32::from_le_bytes(data[..4].try_into().unwrap())
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

struct ConstructionData {
    kidoku_table: Vec<usize>,
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
    };

    (cd, uncompressed)
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
                let offset = (count >> 4) as usize;
                let count = ((count & 0x0f) + 2) as usize;
                let start = d - offset;
                // FIXME: for some reason `copy_within` does not work here:
                // needs some investigation
                for i in 0..count {
                    dst[d + i] = dst[start + i];
                }
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