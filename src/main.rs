extern crate byteorder;

mod cpu;
mod rom;

use cpu::{Cpu, Interrupt};
use rom::Rom;

use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let rom_path = env::args().nth(1).expect("Path to ROM");
    let mut rom_file = File::open(rom_path).expect("ROM file not found");
    let mut rom_bytes: Vec<u8> = Vec::new();
    rom_file.read_to_end(&mut rom_bytes).expect("Could not read ROM");
    let rom = Rom::from(&rom_bytes);
    println!("ROM {:?}", rom);

    let mut cpu = Cpu::new(&rom);
    cpu.interrupt(Interrupt::Reset);
    println!("CPU {:?}", cpu);
}
