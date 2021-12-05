use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Clone)]
struct Point(i32, i32);
#[derive(Debug)]
enum Segment {
    Horizontal { y: i32, x_min: i32, x_max: i32 },
    Vertical { x: i32, y_min: i32, y_max: i32 },
}

impl Point {
    fn mov(&self, direction: &str, length: i32) -> Point {
        match direction {
            "U" => Point(self.0, self.1 - length),
            "D" => Point(self.0, self.1 + length),
            "L" => Point(self.0 - length, self.1),
            "R" => Point(self.0 + length, self.1),
            _ => panic!("Unexpected direction {}", direction),
        }
    }

    fn length(&self) -> i32 {
        self.0.abs() + self.1.abs()
    }
}

impl Segment {
    fn parse_vec(text: &str) -> Vec<Segment> {
        let mut curr_point = Point(0, 0);
        let mut result: Vec<Segment> = Vec::new();
        for instruction in text.split(",") {
            let direction = &instruction[..1];
            let length = instruction[1..]
                .parse::<i32>()
                .expect("Unable to parse direction");
            let Point(x, y) = curr_point;
            result.push(match direction {
                "U" => Segment::Vertical {
                    x: x,
                    y_min: y - length,
                    y_max: y,
                },
                "D" => Segment::Vertical {
                    x: x,
                    y_min: y,
                    y_max: y + length,
                },
                "L" => Segment::Horizontal {
                    y: y,
                    x_min: x - length,
                    x_max: x,
                },
                "R" => Segment::Horizontal {
                    y: y,
                    x_min: x,
                    x_max: x + length,
                },
                _ => panic!("Unexpected direction {}", direction),
            });
            let next_point = curr_point.mov(direction, length);
            curr_point = next_point;
        }
        result
    }

    fn find_intersection_h_v(
        x: i32,
        y_min: i32,
        y_max: i32,
        y: i32,
        x_min: i32,
        x_max: i32,
    ) -> Option<Point> {
        if x >= x_min && x <= x_max && y >= y_min && y <= y_max {
            Option::Some(Point(x, y))
        } else {
            Option::None
        }
    }

    fn find_intersection(first: &Segment, second: &Segment) -> Option<Point> {
        match first {
            Segment::Horizontal { y, x_min, x_max } => match second {
                Segment::Horizontal { .. } => Option::None,
                Segment::Vertical { x, y_min, y_max } => {
                    Segment::find_intersection_h_v(*x, *y_min, *y_max, *y, *x_min, *x_max)
                }
            },
            Segment::Vertical { x, y_min, y_max } => match second {
                Segment::Vertical { .. } => Option::None,
                Segment::Horizontal { y, x_min, x_max } => {
                    Segment::find_intersection_h_v(*x, *y_min, *y_max, *y, *x_min, *x_max)
                }
            },
        }
    }
}

fn all_intersections(line1: &Vec<Segment>, line2: &Vec<Segment>) -> Vec<Point> {
    let mut res: Vec<Point> = Vec::new();
    for segment1 in line1 {
        for segment2 in line2 {
            match Segment::find_intersection(segment1, segment2) {
                Some(p) => res.push(p),
                None => {}
            }
        }
    }
    res
}

fn main() {
    // let line1 = Segment::parse_vec("R75,D30,R83,U83,L12,D49,R71,U7,L72");
    // let line2 = Segment::parse_vec("U62,R66,U55,R34,D71,R55,D58,R83");
    let text_lines =
        io::BufReader::new(fs::File::open("./input.txt").expect("Unable to open file"))
            .lines()
            .map(|x| Segment::parse_vec(&x.unwrap()))
            .collect::<Vec<Vec<Segment>>>();
    let line1 = &text_lines[0];
    let line2 = &text_lines[1];
    let intersetions = all_intersections(line1, line2);
    let min_dist = intersetions
        .iter()
        .map(|x| x.length())
        .filter(|x| *x != 0)
        .min();
    println!("{:?}", min_dist);
}
