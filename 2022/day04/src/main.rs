use std::{fs, io::Result};

type Range = (u32, u32);

fn parse_range(input: &str) -> Range {
    let mut items = input.split("-");
    let from = items
        .next()
        .expect("Should find from")
        .parse::<u32>()
        .expect("Should parse from");
    let to = items
        .next()
        .expect("Should find to")
        .parse::<u32>()
        .expect("Should parse to");
    (from, to)
}

fn parse_line(line: &str) -> (Range, Range) {
    let mut split = line.split(",");
    let fst = split.next().expect("Should parse first range");
    let snd = split.next().expect("Should parse second range");
    (parse_range(fst), parse_range(snd))
}

fn contains(fst: Range, snd: Range) -> bool {
    fst.0 <= snd.0 && fst.1 >= snd.1
}

fn overlaps(fst: Range, snd: Range) -> bool {
    fst.0 <= snd.0 && fst.1 >= snd.0 || fst.0 >= snd.1 && fst.1 <= snd.1
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let lines = input.lines().map(parse_line).collect::<Vec<_>>();
    let part1 = lines
        .iter()
        .filter(|(fst, snd)| contains(*fst, *snd) || contains(*snd, *fst))
        .count();
    println!("part1: {part1}"); // 477

    let part2 = lines
        .iter()
        .filter(|(fst, snd)| overlaps(*fst, *snd) || overlaps(*snd, *fst))
        .count();
    println!("part2: {part2}"); // 830

    Ok(())
}
