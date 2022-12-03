use core::slice::Iter;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn part1(lines: Iter<(char, char)>) -> i32 {
    lines.fold(0, |score, (theirs, yours)| {
        score
            + match yours {
                'X' => 1,
                'Y' => 2,
                'Z' => 3,
                _ => panic!("Unknown symbol"),
            }
            + match (theirs, yours) {
                ('A', 'X') => 3,
                ('A', 'Y') => 6,
                ('A', 'Z') => 0,
                ('B', 'X') => 0,
                ('B', 'Y') => 3,
                ('B', 'Z') => 6,
                ('C', 'X') => 6,
                ('C', 'Y') => 0,
                ('C', 'Z') => 3,
                _ => panic!("Unknown combination"),
            }
    })
}

fn part2(lines: Iter<(char, char)>) -> i32 {
    lines.fold(0, |score, (theirs, yours)| {
        score
            + match yours {
                'X' => 0,
                'Y' => 3,
                'Z' => 6,
                _ => panic!("Unknown symbol"),
            }
            + match (theirs, yours) {
                ('A', 'X') => 3,
                ('A', 'Y') => 1,
                ('A', 'Z') => 2,
                ('B', 'X') => 1,
                ('B', 'Y') => 2,
                ('B', 'Z') => 3,
                ('C', 'X') => 2,
                ('C', 'Y') => 3,
                ('C', 'Z') => 1,
                _ => panic!("Unknown combination"),
            }
    })
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let lines = reader
        .lines()
        .map(|x| x.unwrap())
        .map(|x| (x.chars().nth(0).unwrap(), x.chars().nth(2).unwrap()))
        .collect::<Vec<_>>();
    println!("part1: {}", part1(lines.iter())); // 10624
    println!("part2: {}", part2(lines.iter())); // 14060
}
