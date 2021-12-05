use std::fs;
use std::io::{self, BufRead};

struct Point(i32, i32);
struct Horizontal {
    y: i32,
    from_x: i32,
    to_x: i32,
}
struct Vertical {
    x: i32,
    from_y: i32,
    to_y: i32,
}
enum Segment {
    H(Horizontal),
    V(Vertical),
}
struct PathSegment(Segment, /* total path length so far */ i32);
struct Intersection {
    len1: i32,
    len2: i32,
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
}

fn parse_wire(text: &str) -> Vec<PathSegment> {
    let mut curr_point = Point(0, 0);
    let mut curr_length = 0;
    let mut result: Vec<PathSegment> = Vec::new();

    for instruction in text.split(",") {
        let direction = &instruction[..1];
        let length = instruction[1..]
            .parse::<i32>()
            .expect("Unable to parse direction");
        let Point(x, y) = curr_point;
        let segment = match direction {
            "U" => Segment::V(Vertical {
                x: x,
                from_y: y,
                to_y: y - length,
            }),
            "D" => Segment::V(Vertical {
                x: x,
                from_y: y,
                to_y: y + length,
            }),
            "L" => Segment::H(Horizontal {
                y: y,
                from_x: x,
                to_x: x - length,
            }),
            "R" => Segment::H(Horizontal {
                y: y,
                from_x: x,
                to_x: x + length,
            }),
            _ => panic!("Unexpected direction {}", direction),
        };
        result.push(PathSegment(segment, curr_length + length));
        let next_point = curr_point.mov(direction, length);
        curr_point = next_point;
        curr_length = curr_length + length;
    }
    result
}

fn min_max<T: std::cmp::Ord + Copy>(a: T, b: T) -> (T, T) {
    let min = a.min(b);
    let max = a.max(b);
    (min, max)
}

impl PathSegment {
    fn length_before(&self) -> i32 {
        self.1 - self.0.length()
    }
}

impl Segment {
    fn length(&self) -> i32 {
        match self {
            Segment::H(h) => (h.from_x - h.to_x).abs(),
            Segment::V(v) => (v.from_y - v.to_y).abs(),
        }
    }

    fn find_intersection_h_v(h: &Horizontal, v: &Vertical) -> Option<Point> {
        let (x_min, x_max) = min_max(h.from_x, h.to_x);
        let (y_min, y_max) = min_max(v.from_y, v.to_y);
        if (x_min..x_max).contains(&v.x) && (y_min..y_max).contains(&h.y) {
            Option::Some(Point(v.x, h.y))
        } else {
            Option::None
        }
    }

    fn find_intersection(first: &PathSegment, second: &PathSegment) -> Option<Intersection> {
        match &first.0 {
            Segment::H(h) => match &second.0 {
                Segment::H(_) => Option::None,
                Segment::V(v) => Segment::find_intersection_h_v(&h, &v).map(|p| Intersection {
                    len1: first.length_before() + (h.from_x - p.0).abs(),
                    len2: second.length_before() + (v.from_y - p.1).abs(),
                }),
            },
            Segment::V(v) => match &second.0 {
                Segment::V(_) => Option::None,
                Segment::H(h) => Segment::find_intersection_h_v(&h, &v).map(|p| Intersection {
                    len1: first.length_before() + (v.from_y - p.1).abs(),
                    len2: second.length_before() + (h.from_x - p.0).abs(),
                }),
            },
        }
    }
}

fn all_intersections(line1: &Vec<PathSegment>, line2: &Vec<PathSegment>) -> Vec<Intersection> {
    let mut res: Vec<Intersection> = Vec::new();
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
    let text_lines =
        io::BufReader::new(fs::File::open("./input.txt").expect("Unable to open file"))
            .lines()
            .map(|x| parse_wire(&x.unwrap()))
            .collect::<Vec<Vec<PathSegment>>>();
    let line1 = &text_lines[0];
    let line2 = &text_lines[1];
    let min_dist = all_intersections(line1, line2)
        .iter()
        .map(|x| x.len1 + x.len2)
        .filter(|x| *x != 0)
        .min();
    println!("{:?}", min_dist)
}
