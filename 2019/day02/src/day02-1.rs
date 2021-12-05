use std::fs;
use std::io::{self, BufRead};

type Program = Vec<usize>;

fn read_input() -> Program {
    io::BufReader::new(fs::File::open("./input.txt").unwrap())
        .lines()
        .nth(0).unwrap().unwrap()
        .split(",")
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<usize>>()
}

fn set_initial_state(input: &mut Program) {
    input[1] = 12;
    input[2] = 2;
}

fn run_program(input: &mut Program, current_address: usize) {
    let opcode = input[current_address];
    if opcode == 99 { return; }

    let r1 = input[current_address + 1];
    let r2 = input[current_address + 2];
    let r3 = input[current_address + 3];
    input[r3] = if opcode == 1 { input[r1] + input[r2] } else { input[r1] * input[r2] };
    run_program(input, current_address + 4);
}

fn main() {
    let mut input = read_input();
    set_initial_state(&mut input);
    run_program(&mut input, 0);
    println!("Value is {}", input[0]);
}
