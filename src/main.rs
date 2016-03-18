extern crate byteorder;

mod apu;
mod cpu;
mod cpu_instr;
mod ppu;
mod rom;

use cpu::{Cpu, Interrupt};
use ppu::Ppu;
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
    println!("CPU {:?}", cpu);

    let mut ppu = Ppu::default();
    println!("PPU {:?}", ppu);

    cpu.interrupt(Interrupt::Reset);
    // cpu.interrupt(Interrupt::Nmi);
    ppu.reg = cpu.exec(ppu.reg);
    for _ in 0..20 {
        ppu.reg = cpu.exec(ppu.reg);
        ppu.exec();
        ppu.exec();
        ppu.exec();
        ppu.exec();
    }

    println!("CPU {:?}", cpu);
}
