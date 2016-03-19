extern crate nes;

use nes::rom::Rom;

use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let rom_path = env::args().nth(1).expect("Path to ROM");
    let mut rom_file = File::open(rom_path).expect("ROM file not found");
    let rom = {
        let mut v = Vec::new();
        rom_file.read_to_end(&mut v).expect("Could not read ROM");
        Rom::from(&v)
    };

    println!("{:?}", rom);
}
