pub(crate) struct XorKey {
    pub(crate) xor: [u8; 16],
    pub(crate) offset: usize,
    pub(crate) length: usize,
}

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


pub(crate) fn detect_key(regname: &[u8]) -> Option<&[XorKey]> {
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
