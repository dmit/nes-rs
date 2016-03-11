extern crate byteorder;

use std::env;
use std::fmt;
use std::fs::File;
use std::io::{Cursor, Read, Write};

use byteorder::{BigEndian, ReadBytesExt};

fn main() {
    let rom_path = env::args().nth(1).expect("Path to ROM");
    let mut rom_file = File::open(rom_path).expect("ROM file not found");
    let mut rom_bytes: Vec<u8> = Vec::new();
    rom_file.read_to_end(&mut rom_bytes);
    let rom = Rom::from(&rom_bytes);
    println!("ROM {:?}", rom);

    let mut cpu = Cpu::new(&rom);
    cpu.interrupt(Interrupt::Reset);
    println!("CPU {:?}", cpu);
}

struct Cpu {
    mem: Memory,
    cur_addr: u16,
}
impl Cpu {
    fn new(rom: &Rom) -> Cpu {
        let mut mem = Memory::default();
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

    fn interrupt(&mut self, int: Interrupt) {
        self.goto(int.addr());
    }
}
impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "cur: {:?}, mem: {:?}", self.cur_addr, self.mem)
    }
}

struct Memory {
    ram: [u8; 0x800],
    ppu_reg: PpuReg,
    apu_reg: ApuReg,
    sram: [u8; 0x2000],
    prg_rom: [u8; 0x8000],
}
impl Memory {
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
impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PPU: {:?}, APU: {:?}", self.ppu_reg, self.apu_reg)
    }
}
impl Default for Memory {
    fn default() -> Memory {
        Memory {
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

// +--------+------+------------------------------------------+
// | Offset | Size | Content(s)                               |
// +--------+------+------------------------------------------+
// |   0    |  3   | 'NES'                                    |
// |   3    |  1   | $1A                                      |
// |   4    |  1   | 16K PRG-ROM page count                   |
// |   5    |  1   | 8K CHR-ROM page count                    |
// |   6    |  1   | ROM Control Byte #1                      |
// |        |      |   %####vTsM                              |
// |        |      |    |  ||||+- 0=Horizontal mirroring      |
// |        |      |    |  ||||   1=Vertical mirroring        |
// |        |      |    |  |||+-- 1=SRAM enabled              |
// |        |      |    |  ||+--- 1=512-byte trainer present  |
// |        |      |    |  |+---- 1=Four-screen mirroring     |
// |        |      |    |  |                                  |
// |        |      |    +--+----- Mapper # (lower 4-bits)     |
// |   7    |  1   | ROM Control Byte #2                      |
// |        |      |   %####0000                              |
// |        |      |    |  |                                  |
// |        |      |    +--+----- Mapper # (upper 4-bits)     |
// |  8-15  |  8   | $00                                      |
// | 16-..  |      | Actual 16K PRG-ROM pages (in linear      |
// |  ...   |      | order). If a trainer exists, it precedes |
// |  ...   |      | the first PRG-ROM page.                  |
// | ..-EOF |      | CHR-ROM pages (in ascending order).      |
// +--------+------+------------------------------------------+
struct Rom {
    prg_cnt: u8,
    chr_cnt: u8,
    mirroring: Mirroring,
    sram_enabled: bool,
    four_screen_mirroring: bool,
    mapper: u8,
    trainer: Option<Vec<u8>>,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}
impl fmt::Debug for Rom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "prg: {}, chr: {}, mirroring: {:?}, sram: {}, four screen mirroring: {}, mapper: \
                {:x}, trainer: {}",
               self.prg_cnt,
               self.chr_cnt,
               self.mirroring,
               self.sram_enabled,
               self.four_screen_mirroring,
               self.mapper,
               self.trainer.is_some())
    }
}
impl Rom {
    fn from(bytes: &[u8]) -> Rom {
        let mut rdr = Cursor::new(bytes);
        let magic = rdr.read_u32::<BigEndian>().expect("Magic bytes");
        if magic != 0x4E45531A {
            panic!("Invalid magic number: {:x}", magic);
        }

        let prg_cnt = rdr.read_u8().unwrap();
        let chr_cnt = rdr.read_u8().unwrap();

        let ctrl1 = rdr.read_u8().unwrap();
        let ctrl2 = rdr.read_u8().unwrap();
        let mirroring = match ctrl1 & 0b1 == 0 {
            true => Mirroring::Vertical,
            false => Mirroring::Horizontal,
        };
        let sram_enabled = ctrl1 & 0b10 != 0;
        let trainer_present = ctrl1 & 0b100 != 0;
        let four_screen_mirroring = ctrl1 & 0b1000 != 0;
        let mapper = ctrl1 >> 4 | (ctrl2 & 0b11110000);

        let trainer: Option<Vec<u8>> = match trainer_present {
            true => {
                let mut vec = vec![0; 512];
                if rdr.read(&mut vec).expect("512-byte trainer") != 512 {
                    panic!("Failed to read 512-byte trainer");
                }
                Some(vec)
            }
            false => None,
        };

        let prg_rom_len: usize = 16 * 1024 * prg_cnt as usize;
        let mut prg_rom = vec![0; prg_rom_len];
        if rdr.read(&mut prg_rom).expect("16kb PRG-ROM") != prg_rom_len {
            panic!("Failed to read {} bytes of PRG-ROM", prg_rom_len);
        }

        let chr_rom_len: usize = 8 * 1024 * chr_cnt as usize;
        let mut chr_rom = vec![0; chr_rom_len];
        if rdr.read(&mut chr_rom).expect("8kb CHR-ROM") != chr_rom_len {
            panic!("Failed to read {} bytes of CHR-ROM", chr_rom_len);
        }

        Rom {
            prg_cnt: prg_cnt,
            chr_cnt: chr_cnt,
            mirroring: mirroring,
            sram_enabled: sram_enabled,
            four_screen_mirroring: four_screen_mirroring,
            mapper: mapper,
            trainer: trainer,
            prg_rom: prg_rom,
            chr_rom: chr_rom,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Mirroring {
    Horizontal,
    Vertical,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Interrupt {
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
