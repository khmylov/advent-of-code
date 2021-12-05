use std::collections::{HashMap, HashSet};
use std::io::{BufReader, BufRead};
use std::fs::File;

type OrbitMap = HashMap<String, String>;
type Path<'a> = Vec<&'a str>;

fn calculate_path<'a>(orbit_map: &'a OrbitMap, planet: &'a str) -> Path<'a> {
    let mut res = Vec::new();
    let mut curr = planet;
    while curr != "COM" {
        res.push(curr);
        curr = orbit_map.get(curr).unwrap();
    }
    res
}

fn part1(orbit_map: &OrbitMap) {
    let sum: usize = orbit_map.iter().map(|p| calculate_path(&orbit_map, p.0).len()).sum();
    println!("{}", sum);
}

fn part2(orbit_map: &OrbitMap) {
    let path1: HashSet<&str> = calculate_path(orbit_map, "YOU").into_iter().skip(1).collect();
    let path2: HashSet<&str> = calculate_path(orbit_map, "SAN").into_iter().skip(1).collect();
    let step_count = path2.symmetric_difference(&path1).count();
    println!("{}", step_count);
}

fn main() {
    let mut orbit_map = HashMap::new();

    for line in BufReader::new(File::open("input.txt").unwrap()).lines().map(|x| x.unwrap()) {
        let split = line.split(")").collect::<Vec<&str>>();
        assert_eq!(
            orbit_map.insert(split[1].to_string(), split[0].to_string()).is_none(),
            true);
    }

    part1(&orbit_map);
    part2(&orbit_map);
}
