use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;

struct Image {
    layers: Vec<Vec<u8>>,
    width: u8,
    height: u8,
}

impl Image {
    fn read() -> Image {
        let mut f = File::open("input.txt").unwrap();
        let mut input = String::new();
        f.read_to_string(&mut input).unwrap();
        let digits: Vec<u8> = input
            .chars()
            .filter_map(|c| c.to_digit(10))
            .map(|x| x as u8)
            .collect();
        let (width, height) = (25, 6);
        let layer_size = width * height;
        let layer_count = digits.len() / width / height;

        let mut layers = vec![];
        for layer_index in 0..layer_count {
            let start = layer_index * layer_size;
            let data = digits[start..(start + layer_size)].iter().cloned();
            let layer = Vec::from_iter(data);
            layers.push(layer);
        }

        Image {
            layers,
            width: width as u8,
            height: height as u8,
        }
    }
}

fn number_of_digits(layer: &Vec<u8>, digit: u8) -> usize {
    layer.iter().filter(|x| **x == digit).count()
}

fn render_pixel(layered: &Vec<u8>) -> u8 {
    let mut result = layered[0];
    for &pixel in layered.iter().skip(1) {
        result = if pixel == 2u8 { result } else { pixel };
    }
    result
}

fn part1(image: &Image) {
    let layer = image
        .layers
        .iter()
        .min_by_key(|l| number_of_digits(&l, 0u8))
        .unwrap();
    println!(
        "{}",
        number_of_digits(&layer, 1u8) * number_of_digits(&layer, 2u8)
    )
}

fn part2(image: &Image) {
    let size = (image.width * image.height) as usize;
    let mut res: Vec<u8> = Vec::with_capacity(size);
    for i in 0..size {
        res.push(render_pixel(
            &image.layers.iter().rev().map(|l| l[i as usize]).collect(),
        ));
    }

    for j in 0..image.height {
        for i in 0..image.width {
            print!("{}", res[(j * image.width + i) as usize]);
        }
        println!("");
    }
}

fn main() {
    let image = Image::read();
    part1(&image);
    part2(&image);
}
