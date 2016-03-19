extern crate byteorder;
extern crate nes;

use nes::apu::Apu;
use nes::cpu::{Cpu, Interrupt};
use nes::ppu::Ppu;
use nes::rom::Rom;

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

    let mut ppu = Ppu::new();
    println!("PPU {:?}", ppu);

    let mut apu = Apu::new();
    println!("APU {:?}", apu);

    let mut cpu = Cpu::new(ppu.reg.clone(), apu.reg.clone(), rom);
    println!("CPU {:?}", cpu);

    cpu.interrupt(Interrupt::Reset);
    for _ in 0..20 {
        let cycles = cpu.exec();

        for _ in 0..(cycles * 4) {
            ppu.exec();
            ppu.exec();
            ppu.exec();
            ppu.exec();
        }

        apu.exec();
    }

    println!("CPU {:?}", cpu);
}
