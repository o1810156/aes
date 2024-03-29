use super::*;

// 大学院講義 セキュリティ基礎の課題を受け追加した分です。
// t-tableによる実装を行えとのこと。

// 空間計算量を犠牲に時間計算量を減らす戦法だったはずがt-tableじゃない方の実装がゴミすぎてどうやらt-tableのほうが速くてデータ使用量も小さいという...
// 雑な考察としてはt-tableはソフトウェア向けの実装ということかも

pub fn aes_16_t(plain: &[u8; 16], key: &[u8; 16]) -> [u8; 16] {
    let mut state = [GF256::new(0); 16];
    let mut cipher_key = [GF256::new(0); 16];
    for i in 0..16 {
        state[i] = GF256::new(plain[i]);
        cipher_key[i] = GF256::new(key[i]);
    }

    let round_keys = generate_round_keys(&cipher_key);
    // round 0
    add_round_key(&mut state, &round_keys[0]);
    // round 1 ~ 9
    for i in 1..10 {
        state = t_table_round(&state);
        add_round_key(&mut state, &round_keys[i]);
    }

    // round 10
    sub_bytes(&mut state);
    shift_rows(&mut state);
    add_round_key(&mut state, &round_keys[10]);

    let mut res = [0u8; 16];
    for i in 0..16 {
        res[i] = state[i].val;
    }

    res
}

const fn generate_t_table() -> [[u8; 256]; 4] {
    #[rustfmt::skip]
    let sbox: [u8;256] = [
        0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
        0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
        0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
        0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
        0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
        0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
        0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
        0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
        0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
        0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
        0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
        0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
        0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
        0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 
        0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
        0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
    ];

    let mut r1 = sbox;
    // r1.iter_mut().for_each(|x| *x = gmul(*x, 0x02));
    let mut i = 0;
    while i < 256 {
        r1[i] = const_gmul(r1[i], 0x02);
        i += 1;
    }
    let r2 = sbox;
    let r3 = sbox;
    let mut r4 = sbox;
    // r4.iter_mut().for_each(|x| *x = gmul(*x, 0x03));
    let mut i = 0;
    while i < 256 {
        r4[i] = const_gmul(r4[i], 0x03);
        i += 1;
    }

    [r1, r2, r3, r4]
}

const T_TABLE: [[u8; 256]; 4] = generate_t_table();

fn rot_byte(table: &[GF256; 4]) -> [GF256; 4] {
    [table[3], table[0], table[1], table[2]]
}

fn t_table_round(state: &[GF256; 16]) -> [GF256; 16] {
    let col0 = t_table_round_sub([state[0], state[5], state[10], state[15]]);
    let col1 = t_table_round_sub([state[1], state[6], state[11], state[12]]);
    let col2 = t_table_round_sub([state[2], state[7], state[8], state[13]]);
    let col3 = t_table_round_sub([state[3], state[4], state[9], state[14]]);

    #[rustfmt::skip]
    let res = [
        col0[0], col1[0], col2[0], col3[0],
        col0[1], col1[1], col2[1], col3[1],
        col0[2], col1[2], col2[2], col3[2],
        col0[3], col1[3], col2[3], col3[3],
    ];

    res
}

fn t_table_round_sub(source: [GF256; 4]) -> [GF256; 4] {
    let mut res = [GF256::new(0); 4];

    for j in 0..4 {
        res[j] += GF256::new(T_TABLE[j][source[3].get_usize()]);
    }

    for i in (0..3).rev() {
        res = rot_byte(&res);
        for j in 0..4 {
            res[j] += GF256::new(T_TABLE[j][source[i].get_usize()]);
        }
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    // すべて縦で読む

    #[test]
    fn test() {
        let text: [u8; 16] = [0; 16];

        #[rustfmt::skip]
        let key: [u8;16] = [
            0x00, 0x04, 0x08, 0x0c,
            0x01, 0x05, 0x09, 0x0d,
            0x02, 0x06, 0x0a, 0x0e,
            0x03, 0x07, 0x0b, 0x0f,
        ];

        #[rustfmt::skip]
        let ans: [u8;16] = [
            0xc6, 0x87, 0x6f, 0xa1,
            0xa1, 0x8f, 0x4f, 0xc8,
            0x3b, 0x5b, 0x81, 0xd8,
            0x37, 0x82, 0x62, 0x79,
        ];
        assert_eq!(aes_16_t(&text, &key), ans);
    }

    #[test]
    fn test2() {
        #[rustfmt::skip]
        let text: [u8;16] = [
            0x00, 0x44, 0x88, 0xcc,
            0x11, 0x55, 0x99, 0xdd,
            0x22, 0x66, 0xaa, 0xee,
            0x33, 0x77, 0xbb, 0xff,
        ];
        #[rustfmt::skip]
        let key: [u8;16] = [
            0x00, 0x04, 0x08, 0x0c,
            0x01, 0x05, 0x09, 0x0d,
            0x02, 0x06, 0x0a, 0x0e,
            0x03, 0x07, 0x0b, 0x0f,
        ];
        #[rustfmt::skip]
        let ans: [u8;16] = [
            0x69, 0x6a, 0xd8, 0x70,
            0xc4, 0x7b, 0xcd, 0xb4,
            0xe0, 0x04, 0xb7, 0xc5,
            0xd8, 0x30, 0x80, 0x5a,
        ];
        assert_eq!(aes_16_t(&text, &key), ans);
    }
}
