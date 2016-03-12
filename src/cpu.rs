use std::fmt;
use std::io::{Read, Write};

use rom::Rom;

pub struct Cpu {
    mem: CpuMemory,
    cur_addr: u16,
}
impl Cpu {
    pub fn new(rom: &Rom) -> Cpu {
        let mut mem = CpuMemory::default();
        for (i, b) in rom.prg_rom.iter().enumerate() {
            mem.prg_rom[i] = *b;
        }

        Cpu {
            mem: mem,
            cur_addr: 0,
        }
    }

    fn goto(&mut self, addr: u16) {
        self.cur_addr = addr;
    }

    pub fn interrupt(&mut self, int: Interrupt) {
        self.goto(int.addr());
    }
}
impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "cur: {:?}, mem: {:?}", self.cur_addr, self.mem)
    }
}

struct CpuMemory {
    ram: [u8; 0x800],
    ppu_reg: PpuReg,
    apu_reg: ApuReg,
    sram: [u8; 0x2000],
    prg_rom: [u8; 0x8000],
}
impl CpuMemory {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x07ff => self.ram[addr as usize],
            0x0800...0x1fff => self.ram[(addr % 0x800) as usize],
            0x2000...0x2007 => self.ppu_reg.read(addr - 0x2000),
            0x2008...0x3fff => self.ppu_reg.read((addr - 0x2008) % 8),
            0x4000...0x401f => self.apu_reg.read(addr - 0x4000),
            0x4020...0x5fff => panic!("expansion rom"),
            0x6000...0x7fff => self.sram[(addr - 0x6000) as usize],
            0x8000...0xffff => self.prg_rom[(addr - 0x8000) as usize],
            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000...0x07ff => self.ram[addr as usize] = value,
            0x0800...0x1fff => self.ram[(addr % 0x800) as usize] = value,
            0x2000...0x2007 => self.ppu_reg.write(addr - 0x2000, value),
            0x2008...0x3fff => self.ppu_reg.write((addr - 0x2008) % 8, value),
            0x4000...0x401f => self.apu_reg.write(addr - 0x4000, value),
            0x4020...0x5fff => panic!("expansion rom"),
            0x6000...0x7fff => self.sram[(addr - 0x6000) as usize] = value,
            0x8000...0xffff => self.prg_rom[(addr - 0x8000) as usize] = value,
            _ => unreachable!(),
        }
    }
}
impl fmt::Debug for CpuMemory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PPU: {:?}, APU: {:?}", self.ppu_reg, self.apu_reg)
    }
}
impl Default for CpuMemory {
    fn default() -> CpuMemory {
        CpuMemory {
            ram: [0; 0x800],
            ppu_reg: PpuReg::default(),
            apu_reg: ApuReg::default(),
            sram: [0; 0x2000],
            prg_rom: [0; 0x8000],
        }
    }
}

#[derive(Debug, Default)]
struct PpuReg {
    ppu_ctrl1: PpuCtrl1,
    ppu_ctrl2: PpuCtrl2,
    ppu_status: PpuStatus,
    spr_addr: u8,
    spr_io: u8,
    vram_addr1: u8,
    vram_addr2: u8,
    vram_io: u8,
}
impl PpuReg {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            2 => self.ppu_status.clone().into(),
            7 => self.vram_io,
            _ => panic!("Not a valid PpuReg address: {:x}", addr),
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        panic!("PpuReg write()");
    }
}

#[derive(Debug, Default)]
struct PpuCtrl1(u8);
impl PpuCtrl1 {
    fn name_table_addr(self) -> u16 {
        match self.0 & 0b11 {
            0b00 => 0x2000,
            0b01 => 0x2400,
            0b10 => 0x2800,
            0b11 => 0x2c00,
            _ => unreachable!(),
        }
    }
}
impl From<u8> for PpuCtrl1 {
    fn from(b: u8) -> Self {
        PpuCtrl1(b)
    }
}
impl Into<u8> for PpuCtrl1 {
    fn into(self) -> u8 {
        self.0
    }
}

#[derive(Debug, Default)]
struct PpuCtrl2(u8);
impl PpuCtrl2 {}
impl From<u8> for PpuCtrl2 {
    fn from(b: u8) -> Self {
        PpuCtrl2(b)
    }
}
impl Into<u8> for PpuCtrl2 {
    fn into(self) -> u8 {
        self.0
    }
}

#[derive(Clone, Debug, Default)]
struct PpuStatus(u8);
impl PpuStatus {}
impl From<u8> for PpuStatus {
    fn from(b: u8) -> Self {
        PpuStatus(b)
    }
}
impl Into<u8> for PpuStatus {
    fn into(self) -> u8 {
        self.0
    }
}

#[derive(Debug, Default)]
struct ApuReg {
    pulse1_ctrl: u8,
    pulse1_ramp_ctrl: u8,
    pulse1_fine_tune: u8,
    pulse1_coarse_tune: u8,
    pulse2_ctrl: u8,
    pulse2_ramp_ctrl: u8,
    pulse2_fine_tune: u8,
    pulse2_coarse_tune: u8,
    triangle_ctrl1: u8,
    triangle_ctrl2: u8,
    triangle_freq1: u8,
    triangle_freq2: u8,
    noise_ctrl: u8,
    _unused: u8,
    noise_freq1: u8,
    noise_freq2: u8,
    delta_mod_ctrl: u8,
    delta_mod_da: u8,
    delta_mod_addr: u8,
    delta_mod_data_len: u8,
    sprite_dma: SpriteDma,
    vert_clock_signal: VertClockSignal,
}
impl ApuReg {
    fn read(&self, addr: u16) -> u8 {
        panic!("ApuRef read()");
    }

    fn write(&mut self, addr: u16, value: u8) {
        panic!("PpuReg write()");
    }
}

#[derive(Debug, Default)]
struct SpriteDma;

#[derive(Debug, Default)]
struct VertClockSignal;

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Interrupt {
    Irq,
    Nmi,
    Reset,
}
impl Interrupt {
    fn addr(&self) -> u16 {
        match *self {
            Interrupt::Irq => 0xfffe,
            Interrupt::Nmi => 0xfffa,
            Interrupt::Reset => 0xfffc,
        }
    }
}
