extern crate nes;

use nes::cpu_instr::Instr;
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

    let mut i = 0;
    while i < rom.prg_rom.len() {
        let addr = 0x8000 + i;
        let bytes = &rom.prg_rom[i..];
        let instr = Instr::from(bytes);
        println!("{:#06x}  {:?}", addr, instr);
        i += 1 + instr.1.payload_len() as usize;
    }
}
