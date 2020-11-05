use aes::*;

fn main() {
    let text: [u8;16] = [0;16];
    let key: [u8;16] = [
        0x00, 0x04, 0x08, 0x0c,
        0x01, 0x05, 0x09, 0x0d,
        0x02, 0x06, 0x0a, 0x0e,
        0x03, 0x07, 0x0b, 0x0f,
    ];

    let gf_text = GF256::from_u8array(&text).unwrap();
    dump_array16(&gf_text, "text");
    let gf_key = GF256::from_u8array(&key).unwrap();
    dump_array16(&gf_key, "key");

    let ans = aes_16(&text, &key);
    let gf_ans = GF256::from_u8array(&ans).unwrap();
    dump_array16(&gf_ans, "Cryptgram");
}
