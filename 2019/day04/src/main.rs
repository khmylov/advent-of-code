use std::convert::TryInto;
type Password = [u8; 6];

fn to_password(number: i32) -> Password {
    let mut curr = number;
    let mut res: Password = [0u8; 6];
    for index in 0..6 {
        res[5 - index] = (curr % 10).try_into().unwrap();
        curr = curr / 10;
    }
    res
}

fn matches1(p: &Password) -> bool {
    let mut prev_value = p[0];
    let mut has_double = false;
    for &x in p.iter().skip(1) {
        if x < prev_value {
            return false;
        }
        if x == prev_value {
            has_double = true;
        }
        prev_value = x;
    }

    has_double
}

fn matches2(p: &Password) -> bool {
    let mut prev_value = p[0];
    let mut has_double = false;
    let mut repeat_count = 0;
    for &x in p.iter().skip(1) {
        if x < prev_value {
            return false;
        }
        if x == prev_value {
            repeat_count += 1;
        } else {
            if repeat_count == 1 {
                has_double = true;
            }
            repeat_count = 0;
        }
        prev_value = x;
    }
    has_double || repeat_count == 1
}

fn filter<F>(f: F) -> usize where F: Fn(&Password) -> bool {
    let iter = 145852..616943;
    iter.map(|x| to_password(x)).filter(f).count()
}

fn main() {
    println!("part1: {}", filter(|x| matches1(&x)));
    println!("part2: {}", filter(|x| matches2(&x))); 
}
