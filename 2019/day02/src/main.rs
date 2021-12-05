use std::fs;
use std::io::{self, BufRead};

type Program = Vec<usize>;

fn read_input() -> Program {
    io::BufReader::new(fs::File::open("./input.txt").unwrap())
        .lines()
        .nth(0).unwrap().unwrap()
        .split(",")
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Program>()
}

fn run_program(input: &mut Program, current_address: usize) -> usize {
    let opcode = input[current_address];
    if opcode == 99 { return input[0]; }

    let r1 = input[current_address + 1];
    let r2 = input[current_address + 2];
    let r3 = input[current_address + 3];
    input[r3] = if opcode == 1 { input[r1] + input[r2] } else { input[r1] * input[r2] };
    run_program(input, current_address + 4)
}

fn main() {
    let input = read_input();
    for noun in 0..99 {
        for verb in 0..99 {
            let mut program = input.clone();
            program[1] = noun;
            program[2] = verb;
            if run_program(&mut program, 0) == 19690720 {
                println!("Found {} {}, answer is {}", noun, verb, 100 * noun + verb);
                return;
            }
        }
    }
}
