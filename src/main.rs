use aes::*;
use rand::prelude::*;
use std::io;
use std::io::Write;
use std::time::Instant;
use t_table::*;

fn main() {
    let mut rng = rand::thread_rng();
    /*
    let text: [u8; 16] = rng.gen();
    let key: [u8; 16] = rng.gen();

    let gf_text = GF256::from_u8array(&text).unwrap();
    dump_array16(&gf_text, "text");
    let gf_key = GF256::from_u8array(&key).unwrap();
    dump_array16(&gf_key, "key");

    let ans = aes_16(&text, &key);
    let gf_ans = GF256::from_u8array(&ans).unwrap();
    dump_array16(&gf_ans, "Cryptgram");

    let ans_t = aes_16_t(&text, &key);
    let gf_ans_t = GF256::from_u8array(&ans_t).unwrap();
    dump_array16(&gf_ans_t, "Cryptgram(t-table)");

    if ans != ans_t {
        println!("Error: aes_16 and aes_16_t are different.");
        return;
    } else {
        println!("Success: aes_16 and aes_16_t are same.");
    }
    */

    loop {
        // Battle Mode
        let mut times_str = String::new();
        print!("How many times? > ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut times_str).unwrap();

        let times = match times_str.trim().parse::<usize>() {
            Ok(num) => num,
            Err(_) => {
                return;
            }
        };

        let mut texts: Vec<[u8; 16]> = Vec::with_capacity(times);
        let mut keys: Vec<[u8; 16]> = Vec::with_capacity(times);

        for _ in 0..times {
            texts.push(rng.gen());
            keys.push(rng.gen());
        }

        // check correctness
        let mut correct = 0;
        for i in 0..times {
            let ans = aes_16(&texts[i], &keys[i]);
            let ans_t = aes_16_t(&texts[i], &keys[i]);
            let ans_dec = inv_aes_16(&ans_t, &keys[i]);

            if ans == ans_t && ans_dec == texts[i] {
                correct += 1;
            }
        }
        println!("correctness: {}/{}", correct, times);

        // battle
        let start = Instant::now();
        for i in 0..times {
            let _ = aes_16(&texts[i], &keys[i]);
        }
        let elapsed = start.elapsed();
        println!("aes_16: {} ms", elapsed.as_millis());

        let start = Instant::now();
        for i in 0..times {
            let _ = aes_16_t(&texts[i], &keys[i]);
        }
        let elapsed = start.elapsed();
        println!("aes_16_t: {} ms", elapsed.as_millis());
    }
}
