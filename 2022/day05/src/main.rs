use std::{fs, io::Result};

struct Instruction {
    crate_count: usize,
    from: usize,
    to: usize,
}

struct State {
    stacks: Vec<Vec<char>>,
    instructions: Vec<Instruction>,
}

fn parse_stacks(input: &str) -> State {
    let stack_count = input
        .lines()
        .take_while(|&x| x.len() > 0)
        .last()
        .expect("Should have at least one line")
        .split(" ")
        .filter(|&x| x.len() > 0)
        .last()
        .expect("Should find last number")
        .parse::<usize>()
        .expect("Should parse as number");

    let mut stacks = Vec::with_capacity(stack_count);
    for _ in 0..stack_count {
        stacks.push(Vec::new());
    }

    for line in input.lines().rev().skip_while(|&x| !x.contains('[')) {
        let mut chars = line.chars();
        for index in 0..stack_count {
            chars.next().expect("Should advance");
            let cr = chars.next().expect("Should advance");
            if cr != ' ' {
                stacks[index].push(cr);
            }
            chars.next().expect("Should advance");
            chars.next();
        }
    }

    let instructions = input
        .lines()
        .skip_while(|&x| !x.starts_with("move"))
        .map(|x| {
            let mut words = x.split(" ");
            words.next().unwrap();
            let crate_count = words.next().unwrap().parse::<usize>().unwrap();
            words.next().unwrap();
            let from = words.next().unwrap().parse::<usize>().unwrap();
            words.next().unwrap();
            let to = words.next().unwrap().parse::<usize>().unwrap();
            Instruction {
                crate_count,
                from,
                to,
            }
        })
        .collect::<Vec<_>>();

    State {
        stacks,
        instructions,
    }
}

fn tops(stacks: &Vec<Vec<char>>) -> String {
    stacks
        .iter()
        .flat_map(|s| s.last())
        .map(|&x| x)
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("")
}

fn part1(state: State) -> String {
    let mut stacks = state.stacks;
    for instruction in state.instructions {
        for _ in 0..instruction.crate_count {
            let cr = stacks[instruction.from - 1].pop().unwrap();
            stacks[instruction.to - 1].push(cr);
        }
    }

    return tops(&stacks);
}

fn part2(state: State) -> String {
    let mut stacks = state.stacks;
    for instruction in state.instructions {
        let from_len = stacks[instruction.from - 1].len();
        let removed = stacks[instruction.from - 1]
            .splice(
                from_len - instruction.crate_count..from_len,
                std::iter::empty(),
            )
            .collect::<Vec<_>>();
        for cr in removed {
            stacks[instruction.to - 1].push(cr);
        }
    }

    return tops(&stacks);
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    println!("part1: {}", part1(parse_stacks(&input))); // FCVRLMVQP
    println!("part2: {}", part2(parse_stacks(&input))); // RWLWGJGFD
    Ok(())
}
