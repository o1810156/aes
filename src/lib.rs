#[macro_use]
extern crate lazy_static;
use std::fmt;
use std::ops::{Add, AddAssign, BitXor, BitXorAssign, Div, Mul};
#[cfg(feature = "t_table")]
pub mod t_table;

// TODO: .to_u8() -> 他の手段を使ってでのu8への変換を可能に
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GF256 {
    val: u8,
}

impl GF256 {
    pub fn new(val: u8) -> GF256 {
        GF256 { val }
    }

    pub fn from_u8array(array: &[u8]) -> Result<[GF256; 16], ()> {
        if array.len() != 16 {
            return Err(());
        }
        let mut res = [GF256::new(0); 16];
        for i in 0..16 {
            res[i] = GF256::new(array[i]);
        }
        Ok(res)
    }

    pub fn get_u8(&self) -> u8 {
        self.val
    }

    pub fn get_usize(&self) -> usize {
        self.val as usize
    }

    // 加算は何があろうとすべて排他的論理和
    fn add_by_xor(a: u8, b: u8) -> GF256 {
        GF256::new(a ^ b)
    }
}

impl fmt::Display for GF256 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:>02x}", self.val)
    }
}

// text book gmul
// 理由: MixColumnsとInvMixColumnで使う積演算だけど8クロックも払うべき演算ではない。
// 必要な数は限られるのでその分を予め計算してしまうのが吉

#[cfg(not(feature = "const_gmul"))]
fn gmul(x: u8, y: u8) -> u8 {
    let mut r: usize = 0;
    let mut a: usize = x as usize;
    let mut b: usize = y as usize;

    // よく見るとaの繰り上がり以外は普通の掛け算
    for _ in 0..8 {
        if (b & 1) == 1 {
            r ^= a;
        }

        let hi_bit_set = a & 0x80;
        a <<= 1;
        // 排他的論理和をうまく機能させるため、足す数となるaを調整する
        // 原始多項式 P(x) = x^8 + x^4+ x^3 + x + 1
        // を利用するので0x1bを使用。
        if hi_bit_set == 0x80 {
            a -= 0xff + 1;
            a ^= 0x1b;
        }

        b >>= 1;
    }

    // 範囲内に収まっているか検査
    if 0xff < r {
        panic!("Something Wrong in Gmul. r = {}", r);
    }

    r as u8
}

#[cfg(any(feature = "const_gmul", feature = "t_table"))]
use const_panic::concat_panic;

#[cfg(any(feature = "const_gmul", feature = "t_table"))]
const fn const_gmul(x: u8, y: u8) -> u8 {
    let mut r: usize = 0;
    let mut a: usize = x as usize;
    let mut b: usize = y as usize;

    // for _ in 0..8 {
    let mut i = 0;
    while i < 8 {
        if (b & 1) == 1 {
            r ^= a;
        }

        let hi_bit_set = a & 0x80;
        a <<= 1;

        if hi_bit_set == 0x80 {
            a -= 0xff + 1;
            a ^= 0x1b;
        }

        b >>= 1;
        i += 1;
    }

    // 範囲内に収まっているか検査
    if 0xff < r {
        concat_panic!("Something Wrong in Gmul. r = {}", r);
    }

    r as u8
}

/* exceeded interpreter step limit (see `#[const_eval_limit]`) と怒られてしまいました... */
/*
#[cfg(feature = "const_gmul")]
const fn gmul_lookup() -> [[u8; 256]; 256] {
    let mut res = [[0; 256]; 256];

    let mut i = 0;
    while i <= 255 {
        let mut j = 255;
        while j >= i {
            let tmp = const_gmul(i as u8, j as u8);
            res[i as usize][j as usize] = tmp;
            res[j as usize][i as usize] = tmp;

            j -= 1;
        }

        i += 1;
    }
    res
}
*/

#[cfg(feature = "const_gmul")]
const fn gmul_lookup_for_2_3() -> [[u8; 256]; 256] {
    let mut res = [[0; 256]; 256];

    let mut i = 0;
    while i <= 255 {
        let res2 = const_gmul(i as u8, 2);
        res[2][i] = res2;
        res[i][2] = res2;
        let res3 = const_gmul(i as u8, 3);
        res[3][i] = res3;
        res[i][3] = res3;

        i += 1;
    }
    res
}

#[cfg(feature = "const_gmul")]
const GMUL_LOOKUP_FOR_2_3: [[u8; 256]; 256] = gmul_lookup_for_2_3();

#[cfg(feature = "const_gmul")]
fn gmul(x: u8, y: u8) -> u8 {
    if x == 2 || x == 3 || y == 2 || y == 3 {
        GMUL_LOOKUP_FOR_2_3[x as usize][y as usize]
    } else {
        const_gmul(x, y)
    }
}

impl Mul for GF256 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        // GF256::new(gmul(self.val, rhs.val))
        GF256::new(gmul(self.val, rhs.val))
    }
}

impl Add for GF256 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        GF256::add_by_xor(self.val, rhs.val)
    }
}

impl AddAssign for GF256 {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl BitXor for GF256 {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        GF256::add_by_xor(self.val, rhs.val)
    }
}

impl BitXorAssign for GF256 {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = GF256::add_by_xor(self.val, rhs.val);
    }
}

fn ginv() -> [u8; 256] {
    let mut res = [0u8; 256];
    for i in 1..=255 {
        if res[i as usize] != 0 {
            continue;
        }
        for j in 1..=255 {
            if gmul(i, j) == 1 {
                res[i as usize] = j;
                res[j as usize] = i;
            }
        }
    }

    res
}

lazy_static! {
    static ref GINV_TABLE: [u8; 256] = ginv();
}

impl Div for GF256 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        self * GF256::new(GINV_TABLE[rhs.val as usize])
    }
}

// #[allow(dead_code)]
pub fn dump_array<T: fmt::Display>(array: &[T], comment: &str) {
    if array.len() != 16 {
        println!("!= 16. so no print.");
        return;
    }
    println!("==== {} ====", comment);
    for i in 0..4 {
        for j in 0..4 {
            print!("{} ", array[4 * i + j]);
        }
        println!("");
    }
    println!("===========");
}

pub fn array2string(array: &[GF256]) -> String {
    if array.len() != 16 {
        return "!= 16. so no print.".to_string();
    }
    let mut s = String::new();
    for i in 0..4 {
        s = format!(
            "{}{} {} {} {}\n",
            s,
            array[4 * i],
            array[4 * i + 1],
            array[4 * i + 2],
            array[4 * i + 3],
        );
    }
    s
}

pub fn dump_array16(array: &[GF256], comment: &str) {
    if array.len() != 16 {
        println!("!= 16. so no print.");
        return;
    }
    println!("==== {} ====", comment);
    for i in 0..4 {
        for j in 0..4 {
            print!("0x{},", array[4 * i + j]);
        }
        println!("");
    }
    println!("===========");
}

#[allow(dead_code)]
fn dump_keys(round_keys: &[[GF256; 16]; 11]) {
    round_keys
        .iter()
        .enumerate()
        .for_each(|(i, key)| dump_array(key, i.to_string().as_str()));
}

/* 参考にしたページ
https://qiita.com/kkent030315/items/ab0792aa1e8948b57490
https://www.youtube.com/watch?v=mlzxpkdXP58
*/

/* stateについて

state[0], state[1], state[2], state[3],
state[4], ...
state[8], ...
state[12], ...

のように格納。書きやすさ、ハードウェア面両方から見てもこちらのほうが効率が良さそうであると考えた。

...けど、もしかしたら縦横逆のほうが良かったかもしれない(大して変わらない説もある)
 */

/*
全体的にハードコードが多いのはご愛嬌
*/

pub fn aes_16(plain: &[u8; 16], key: &[u8; 16]) -> [u8; 16] {
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
        sub_bytes(&mut state);
        shift_rows(&mut state);
        mix_columns(&mut state);
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

pub fn inv_aes_16(cg: &[u8; 16], key: &[u8; 16]) -> [u8; 16] {
    // cgはcryptogramの略
    let mut state = [GF256::new(0); 16];
    let mut cipher_key = [GF256::new(0); 16];
    for i in 0..16 {
        state[i] = GF256::new(cg[i]);
        cipher_key[i] = GF256::new(key[i]);
    }

    let round_keys = generate_round_keys(&cipher_key);
    // round 10
    add_round_key(&mut state, &round_keys[10]);
    inv_shift_rows(&mut state);
    inv_sub_bytes(&mut state);

    // round 9 ～ 1
    for i in (1..10).rev() {
        add_round_key(&mut state, &round_keys[i]);
        inv_mix_columns(&mut state);
        inv_shift_rows(&mut state);
        inv_sub_bytes(&mut state);
    }

    // round 0
    add_round_key(&mut state, &round_keys[0]);

    let mut res = [0u8; 16];
    for i in 0..16 {
        res[i] = state[i].val;
    }

    res
}

pub fn aes_16_verbose(plain: &[u8; 16], key: &[u8; 16]) -> [u8; 16] {
    let mut state = [GF256::new(0); 16];
    let mut cipher_key = [GF256::new(0); 16];
    for i in 0..16 {
        state[i] = GF256::new(plain[i]);
        cipher_key[i] = GF256::new(key[i]);
    }

    let round_keys = generate_round_keys(&cipher_key);
    dump_keys(&round_keys);
    // round 0
    dump_array(&state, "R0 ini");
    add_round_key(&mut state, &round_keys[0]);
    dump_array(&state, "R0 ark");
    // round 1 ~ 9
    for i in 1..10 {
        dump_array(&state, format!("R{} ini", i).as_str());
        sub_bytes(&mut state);
        dump_array(&state, format!("R{} sb", i).as_str());
        shift_rows(&mut state);
        dump_array(&state, format!("R{} sr", i).as_str());
        mix_columns(&mut state);
        dump_array(&state, format!("R{} mc", i).as_str());
        add_round_key(&mut state, &round_keys[i]);
        dump_array(&state, format!("R{} ark", i).as_str());
    }

    // round 10
    dump_array(&state, "R10 ini");
    sub_bytes(&mut state);
    dump_array(&state, "R10 sb");
    shift_rows(&mut state);
    dump_array(&state, "R10 sr");
    add_round_key(&mut state, &round_keys[10]);
    dump_array(&state, "R10 ark");

    let mut res = [0u8; 16];
    for i in 0..16 {
        res[i] = state[i].val;
    }

    res
}

pub fn aes_16_bloken(plain: &[u8; 16], key: &[u8; 16], verbose: bool) -> [u8; 16] {
    let mut state = [GF256::new(0); 16];
    let mut cipher_key = [GF256::new(0); 16];
    for i in 0..16 {
        state[i] = GF256::new(plain[i]);
        cipher_key[i] = GF256::new(key[i]);
    }

    let round_keys = generate_round_keys(&cipher_key);
    if verbose {
        dump_keys(&round_keys);
    }
    // round 0
    if verbose {
        dump_array(&state, "R0 ini");
    }
    add_round_key(&mut state, &round_keys[0]);
    if verbose {
        dump_array(&state, "R0 ark");
    }
    // round 1 ~ 8
    // ここに故障を注入するとブワッと広がるのでround 10ではぐちゃぐちゃになりほぼ無意味
    for i in 1..9 {
        if verbose {
            dump_array(&state, format!("R{} ini", i).as_str());
        }
        sub_bytes(&mut state);
        if verbose {
            dump_array(&state, format!("R{} sb", i).as_str());
        }
        shift_rows(&mut state);
        if verbose {
            dump_array(&state, format!("R{} sr", i).as_str());
        }
        mix_columns(&mut state);
        if verbose {
            dump_array(&state, format!("R{} mc", i).as_str());
        }
        add_round_key(&mut state, &round_keys[i]);
        if verbose {
            dump_array(&state, format!("R{} ark", i).as_str());
        }
    }

    // round 9
    // ここのmix_columnより前に入れるといい感じになる(どこでも変わらない)

    state[0] ^= GF256::new(0xff); // 故障注入
    if verbose {
        dump_array(&state, format!("R9 ini").as_str());
    }
    sub_bytes(&mut state);
    if verbose {
        dump_array(&state, format!("R9 sb").as_str());
    }
    shift_rows(&mut state);
    if verbose {
        dump_array(&state, format!("R9 sr").as_str());
    }
    mix_columns(&mut state);
    if verbose {
        dump_array(&state, format!("R9 mc").as_str());
    }
    add_round_key(&mut state, &round_keys[9]);
    if verbose {
        dump_array(&state, format!("R9 ark").as_str());
    }

    // round 10
    // ここで故障を注入した場合

    if verbose {
        dump_array(&state, "R10 ini");
    }
    sub_bytes(&mut state);
    if verbose {
        dump_array(&state, "R10 sb");
    }
    shift_rows(&mut state);
    if verbose {
        dump_array(&state, "R10 sr");
    }
    add_round_key(&mut state, &round_keys[10]);
    if verbose {
        dump_array(&state, "R10 ark");
    }

    let mut res = [0u8; 16];
    for i in 0..16 {
        res[i] = state[i].val;
    }

    res
}

pub fn sub_bytes(state: &mut [GF256; 16]) {
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

    for i in 0..16 {
        state[i] = GF256::new(sbox[state[i].val as usize]);
    }
}

pub fn inv_sub_bytes(state: &mut [GF256; 16]) {
    #[rustfmt::skip]
    let inv_s: [u8;256] = [
        0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB,
        0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB,
        0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E,
        0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25,
        0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92,
        0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84,
        0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06,
        0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B,
        0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73,
        0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E,
        0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B,
        0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4,
        0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F,
        0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF,
        0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61,
        0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D
    ];

    for i in 0..16 {
        state[i] = GF256::new(inv_s[state[i].val as usize]);
    }
}

pub fn shift_rows(state: &mut [GF256; 16]) {
    // 1行目
    /*
    なにもせず
     */

    // 2行目
    let tmp = state[4];
    state[4] = state[5];
    state[5] = state[6];
    state[6] = state[7];
    state[7] = tmp;

    // 3行目 実質ただのスワップ
    let tmp = state[8];
    state[8] = state[10];
    state[10] = tmp;
    let tmp = state[9];
    state[9] = state[11];
    state[11] = tmp;

    // 4行目
    let tmp = state[15];
    state[15] = state[14];
    state[14] = state[13];
    state[13] = state[12];
    state[12] = tmp;
}

pub fn inv_shift_rows(state: &mut [GF256; 16]) {
    // 1行目
    /*
    なにもせず
     */

    // 2行目
    let tmp = state[7];
    state[7] = state[6];
    state[6] = state[5];
    state[5] = state[4];
    state[4] = tmp;

    // 3行目 実質ただのスワップ
    // つまり、逆関数でも書き換え不要
    let tmp = state[8];
    state[8] = state[10];
    state[10] = tmp;
    let tmp = state[9];
    state[9] = state[11];
    state[11] = tmp;

    // 4行目
    let tmp = state[12];
    state[12] = state[13];
    state[13] = state[14];
    state[14] = state[15];
    state[15] = tmp;
}

pub fn mix_columns(state: &mut [GF256; 16]) {
    let g2 = GF256::new(2);
    let g3 = GF256::new(3);
    for i in 0..4 {
        let b0 = state[i];
        let b1 = state[i + 4];
        let b2 = state[i + 8];
        let b3 = state[i + 12];

        state[i] = (g2 * b0) ^ (g3 * b1) ^ b2 ^ b3;
        state[i + 4] = b0 ^ (g2 * b1) ^ (g3 * b2) ^ b3;
        state[i + 8] = b0 ^ b1 ^ (g2 * b2) ^ (g3 * b3);
        state[i + 12] = (g3 * b0) ^ b1 ^ b2 ^ (g2 * b3);
    }
}

pub fn inv_mix_columns(state: &mut [GF256; 16]) {
    // 学部時代 セキュ実験自分
    // 「なぜ9, 11, 13, 14なのかはよくわからん
    // 行列計算する...? 逆行列か...?」
    // 大学院時代 セキュ基礎自分
    // 「Yes. According to the slide of the class,
    // "For the inverse of the MixColumns operation, the inversion matrix is required."」
    let g9 = GF256::new(9);
    let g11 = GF256::new(11);
    let g13 = GF256::new(13);
    let g14 = GF256::new(14);
    for i in 0..4 {
        let b0 = state[i];
        let b1 = state[i + 4];
        let b2 = state[i + 8];
        let b3 = state[i + 12];

        state[i] = (g14 * b0) ^ (g11 * b1) ^ (g13 * b2) ^ (g9 * b3);
        state[i + 4] = (g9 * b0) ^ (g14 * b1) ^ (g11 * b2) ^ (g13 * b3);
        state[i + 8] = (g13 * b0) ^ (g9 * b1) ^ (g14 * b2) ^ (g11 * b3);
        state[i + 12] = (g11 * b0) ^ (g13 * b1) ^ (g9 * b2) ^ (g14 * b3);
    }
}

// 実質、関数名をそのままコードにしただけである
// add_round_keyの逆関数としてadd_round_keyを使って良い...?なぜ...?
// なぜなら、同じものを2度足すともとに戻るため。それはすなわち逆関数である。
pub fn add_round_key(state: &mut [GF256; 16], round_key: &[GF256; 16]) {
    for i in 0..16 {
        state[i] ^= round_key[i];
    }
}

pub fn generate_round_keys(cipher_key: &[GF256; 16]) -> [[GF256; 16]; 11] {
    // よくわからんけど0番も用意されてる
    #[rustfmt::skip]
    let rcon = [
        0x01,0x01,0x02,0x04,
        0x08,0x10,0x20,0x40,
        0x80,0x1b,0x36,0x6c,
        0xd8,0xab,0x4d,0x9a
    ];

    #[rustfmt::skip]
    let sbox: [u8;256] = [
        0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
        0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
        0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
        0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
        0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
        0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
        0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
        0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16
    ];

    let mut res = [[GF256::new(0); 16]; 11];

    for i in 0..16 {
        res[0][i] = cipher_key[i];
    }

    /* hint

     0  1  2  3
     4  5  6  7
     8  9 10 11
    12 13 14 15

     */

    for i in 1..11 {
        for j in 0..4 {
            res[i][4 * j] = res[i - 1][4 * j + 3];
        }

        // rot_word
        let tmp = res[i][0];
        res[i][0] = res[i][4];
        res[i][4] = res[i][8];
        res[i][8] = res[i][12];
        res[i][12] = tmp;

        // sub_word
        for j in 0..4 {
            res[i][4 * j] = GF256::new(sbox[res[i][4 * j].val as usize]);
        }

        // rcon
        res[i][0] ^= GF256::new(rcon[i]);

        // 2つ前と足し合わせ
        for k in 0..4 {
            res[i][4 * k] ^= res[i - 1][4 * k];
        }

        // other cols
        for j in 1..4 {
            for k in 0..4 {
                res[i][j + 4 * k] = res[i - 1][j + 4 * k] ^ res[i][j + 4 * k - 1];
            }
        }
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gf256_test() {
        let a = GF256::new(0);
        let b = GF256::new(1);
        assert!(a == GF256::new(0));
        assert!(a != b);
        let c = GF256::new(1);
        assert!(b == c);
    }

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
        assert_eq!(aes_16(&text, &key), ans);
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
        assert_eq!(aes_16(&text, &key), ans);
    }

    #[test]
    fn test3() {
        #[rustfmt::skip]
        let cg: [u8;16] = [
            0xc6, 0x87, 0x6f, 0xa1,
            0xa1, 0x8f, 0x4f, 0xc8,
            0x3b, 0x5b, 0x81, 0xd8,
            0x37, 0x82, 0x62, 0x79,
        ];
        #[rustfmt::skip]
        let key: [u8;16] = [
            0x00, 0x04, 0x08, 0x0c,
            0x01, 0x05, 0x09, 0x0d,
            0x02, 0x06, 0x0a, 0x0e,
            0x03, 0x07, 0x0b, 0x0f,
        ];
        let ans: [u8; 16] = [0; 16];
        assert_eq!(inv_aes_16(&cg, &key), ans);
    }

    #[test]
    fn test4() {
        #[rustfmt::skip]
        let ans: [u8;16] = [
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
        let cg: [u8;16] = [
            0x69, 0x6a, 0xd8, 0x70,
            0xc4, 0x7b, 0xcd, 0xb4,
            0xe0, 0x04, 0xb7, 0xc5,
            0xd8, 0x30, 0x80, 0x5a,
        ];
        assert_eq!(inv_aes_16(&cg, &key), ans);
    }
}
