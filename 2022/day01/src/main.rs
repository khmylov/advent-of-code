use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").unwrap();
    let mut elves = contents
        .split("\n\n")
        .map(|paragraph| {
            paragraph
                .split("\n")
                .filter(|line| line.len() > 0)
                .map(|line| line.parse::<i32>().unwrap())
                .sum::<i32>()
        })
        .collect::<Vec<_>>();
    let part1 = elves.iter().max().unwrap();
    println!("part1: {part1}"); // 67658

    elves.sort_by(|a, b| b.cmp(a));
    let part2: i32 = elves.iter().take(3).sum();

    println!("part2: {part2}"); // 200158
}
