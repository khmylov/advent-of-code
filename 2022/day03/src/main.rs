use std::{collections::HashSet, fs};

fn to_number(x: char) -> u32 {
    if x.is_ascii_lowercase() {
        x as u32 - 97 + 1
    } else {
        x as u32 - 65 + 27
    }
}

fn main() -> std::io::Result<()> {
    let input = fs::read_to_string("input.txt")?;

    let part1: u32 = input
        .lines()
        .map(|line| {
            let length = line.len();
            let left = HashSet::<_>::from_iter(line[0..length / 2].chars());
            let right = HashSet::from_iter(line[length / 2..].chars());
            let both = left
                .intersection(&right)
                .next()
                .expect("Should have one matching item");
            to_number(*both)
        })
        .sum();
    println!("part1: {}", part1); // 8176

    let mut part2_lines = input.lines();
    let mut part2 = 0;
    loop {
        match part2_lines.next() {
            None => break,
            Some(line) => {
                let l1 = HashSet::<_>::from_iter(line.chars());
                let l2 = HashSet::<_>::from_iter(part2_lines.next().unwrap().chars());
                let l3 = HashSet::<_>::from_iter(part2_lines.next().unwrap().chars());

                let res = HashSet::<_>::from_iter(l1.intersection(&l2).cloned());
                let mut res = res.intersection(&l3);
                let res = res.next().expect("Should have one matching item");
                part2 += to_number(*res);
            }
        }
    }

    println!("part2: {}", part2); // 2689

    Ok(())
}
