use std::fs;
use std::io::{self, BufRead};

fn requirement(mass: i32) -> i32 {
    ((mass as f64) / 3f64).floor() as i32 - 2
}

fn part1() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let file = fs::File::open("./input.txt")?;
    let answer = io::BufReader::new(file)
        .lines()
        .map(|x| x.unwrap().parse::<i32>().unwrap())
        .map(|x| requirement(x))
        .sum::<i32>();

    println!("{}", answer);
    Ok(())
}

fn requirement_loop(mass: i32) -> i32 {
    let res = requirement(mass);
    if res > 0 {
        res + requirement_loop(res)
    } else {
        0
    }
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let file = fs::File::open("./input.txt")?;
    let answer = io::BufReader::new(file)
        .lines()
        .map(|x| x.unwrap().parse::<i32>().unwrap())
        .map(|x| requirement_loop(x))
        .sum::<i32>();

    println!("{}", answer);
    Ok(())
}
