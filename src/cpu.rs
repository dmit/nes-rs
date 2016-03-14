use std::fmt;
use std::io::{Read, Write};

use rom::Rom;

pub struct Cpu {
    reg: Reg,
    ram: [u8; 0x800],
    ppu_reg: PpuReg,
    apu_reg: ApuReg,
    sram: [u8; 0x2000],
    prg_rom: [u8; 0x8000],
}
impl Cpu {
    pub fn new(rom: &Rom) -> Cpu {
        let mut prg_rom = [0u8; 0x8000];
        for (i, b) in rom.prg_rom.iter().enumerate() {
            prg_rom[i] = *b;
        }

        Cpu {
            reg: Reg::default(),
            ram: [0; 0x800],
            ppu_reg: PpuReg::default(),
            apu_reg: ApuReg::default(),
            sram: [0; 0x2000],
            prg_rom: prg_rom,
        }
    }

    fn read_u8(&self, addr: u16) -> u8 {
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

    fn read_i8(&self, addr: u16) -> i8 {
        self.read_u8(addr) as i8
    }

    fn read_u16(&self, addr: u16) -> u16 {
        self.read_u8(addr) as u16 | (self.read_u8(addr + 1) as u16) << 8
    }

    fn write_u8(&mut self, addr: u16, value: u8) {
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

    fn pop_u8(&mut self) -> u8 {
        let addr = self.reg.sp.wrapping_add(1);
        self.reg.sp = addr;
        self.read_u8(0x100 | addr as u16)
    }

    fn pop_u16(&mut self) -> u16 {
        let lo = self.pop_u8();
        let hi = self.pop_u8();
        lo as u16 | (hi as u16) << 8
    }

    fn push_u8(&mut self, value: u8) {
        let addr = 0x100 | self.reg.sp as u16;
        self.write_u8(addr, value);
        self.reg.sp = self.reg.sp.wrapping_sub(1);
    }

    pub fn exec(&mut self) {
        let addr = self.reg.pc;
        let opcode = self.read_u8(addr);
        let instr = Instr::from(opcode);
        let b1 = self.read_u8(addr + 1);
        let b2 = self.read_u8(addr + 2);
        println!("{:#06x} {:#04x} {:?} -- {:02x} {:02x}",
                 addr,
                 opcode,
                 instr,
                 b1,
                 b2);

        self.reg.pc = self.reg.pc.wrapping_add(1);

        match instr {
            Instr::And(m) => {
                let mem = self.payload_u8(m);
                let val = self.reg.acc & mem;

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.acc = val;
            }
            Instr::Asl(m) => {
                let addr = self.payload_u16(m);
                let old_val = self.read_u8(addr);
                let new_val = old_val << 1;

                self.reg.status.carry = old_val & 0b1000_0000 != 0;
                self.reg.status.neg = new_val & 0b1000_0000 != 0;
                self.reg.status.zero = new_val == 0;

                self.write_u8(addr, new_val);
            }
            Instr::Bcs(m) => {
                let addr = self.payload_u16(m);
                if self.reg.status.carry {
                    self.reg.pc = addr;
                }
            }
            Instr::Beq(m) => {
                let addr = self.payload_u16(m);
                if self.reg.status.zero {
                    self.reg.pc = addr;
                }
            }
            Instr::Bit(m) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.overflow = val & 0b0100_0000 != 0;
                self.reg.status.zero = val & self.reg.acc == 0;
            }
            Instr::Bmi(m) => {
                let addr = self.payload_u16(m);
                if self.reg.status.neg {
                    self.reg.pc = addr;
                }
            }
            Instr::Bne(m) => {
                let addr = self.payload_u16(m);
                if !self.reg.status.zero {
                    self.reg.pc = addr;
                }
            }
            Instr::Bpl(m) => {
                let addr = self.payload_addr(m);
                if !self.reg.status.neg {
                    self.reg.pc = addr;
                }
            }
            Instr::Brk => self.interrupt(Interrupt::Brk),
            Instr::Cld => self.reg.status.decimal_mode = false,
            Instr::Cmp(m) => {
                let mem = self.payload_u8(m);
                let val = self.reg.acc - mem;

                self.reg.status.carry = self.reg.acc >= mem;
                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;
            }
            Instr::Dec(m) => {
                let addr = self.payload_u16(m);
                let val = self.read_u8(addr).wrapping_sub(1);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.write_u8(addr, val);
            }
            Instr::Dex => {
                self.reg.x = self.reg.x.wrapping_sub(1);
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;
            }
            Instr::Dey => {
                self.reg.y = self.reg.y.wrapping_sub(1);
                self.reg.status.neg = self.reg.y & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.y == 0;
            }
            Instr::Inc(m) => {
                let addr = self.payload_u16(m);
                let val = self.read_u8(addr).wrapping_add(1);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.write_u8(addr, val);
            }
            Instr::Inx => {
                self.reg.x = self.reg.x.wrapping_add(1);
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;
            }
            Instr::Iny => {
                self.reg.y = self.reg.y.wrapping_add(1);
                self.reg.status.neg = self.reg.y & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.y == 0;
            }
            Instr::Lda(m) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.acc = val;
            }
            Instr::Ldx(m) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.x = val;
            }
            Instr::Ldy(m) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.y = val;
            }
            Instr::Ora(m) => {
                let mem = self.payload_u8(m);
                let val = self.reg.acc | mem;

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;
            }
            Instr::Rts => self.reg.pc = self.pop_u16().wrapping_add(1),
            Instr::Sei => self.reg.status.irq_disabled = true,
            Instr::Sta(m) => {
                let addr = self.payload_addr(m);
                let val = self.reg.acc;
                self.write_u8(addr, val);
            }
            Instr::Stx(m) => {
                let addr = self.payload_addr(m);
                let val = self.reg.x;
                self.write_u8(addr, val);
            }
            Instr::Sty(m) => {
                let addr = self.payload_addr(m);
                let val = self.reg.y;
                self.write_u8(addr, val);
            }
            Instr::Txa => {
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;

                self.reg.acc = self.reg.x;
            }
            Instr::Txs => {
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;

                self.reg.sp = self.reg.x;
            }
            Instr::Tya => {
                self.reg.status.neg = self.reg.y & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.y == 0;

                self.reg.acc = self.reg.y;
            }
            _ => {
                panic!("Unknown instruction {:?} from opcode {:#x} at {:#x}",
                       instr,
                       opcode,
                       addr)
            }
        }
    }

    fn payload_addr(&mut self, mode: AddrMode) -> u16 {
        let addr = match mode {
            AddrMode::Abs => self.read_u16(self.reg.pc),
            AddrMode::AbsX => {
                let offset = self.read_u8(self.reg.pc);
                (self.reg.x as u16).wrapping_add(offset as u16)
            }
            AddrMode::AbsY => {
                let offset = self.read_u8(self.reg.pc);
                (self.reg.y as u16).wrapping_add(offset as u16)
            }
            AddrMode::Immediate => self.reg.pc,
            AddrMode::Ind => {
                let addr = self.read_u16(self.reg.pc);
                self.read_u16(addr)
            }
            AddrMode::IndX => {
                let zero_offset = self.read_u8(self.reg.pc);
                let zero_addr = self.reg.x as u16 + zero_offset as u16;
                self.read_u16(zero_addr)
            }
            AddrMode::IndY => {
                let zero_offset = self.read_u8(self.reg.pc);
                let zero_addr = self.read_u16(zero_offset as u16);
                zero_addr + self.reg.y as u16
            }
            AddrMode::Rel => {
                let offset = self.read_i8(self.reg.pc);
                if offset < 0 {
                    self.reg
                        .pc
                        .wrapping_add(1)
                        .wrapping_sub(offset.wrapping_neg() as u16)
                } else {
                    self.reg
                        .pc
                        .wrapping_add(1)
                        .wrapping_add(offset as u16)
                }
            }
            AddrMode::Zero => self.read_u8(self.reg.pc) as u16,
            AddrMode::ZeroX => {
                let offset = self.read_u8(self.reg.pc);
                self.reg.x.wrapping_add(offset) as u16
            }
            AddrMode::ZeroY => {
                let offset = self.read_u8(self.reg.pc);
                self.reg.y.wrapping_add(offset) as u16
            }
            _ => panic!("Unsupported address mode: {:?}", mode),
        };

        self.reg.pc = self.reg.pc.wrapping_add(mode.payload_len());

        addr
    }

    fn payload_u8(&mut self, mode: AddrMode) -> u8 {
        let addr = self.payload_addr(mode);
        self.read_u8(addr)
    }

    fn payload_u16(&mut self, mode: AddrMode) -> u16 {
        let addr = self.payload_addr(mode);
        self.read_u16(addr)
    }

    pub fn interrupt(&mut self, int: Interrupt) {
        if self.reg.status.irq_disabled && int == Interrupt::Irq {
            return;
        }

        let vec_addr = int.addr();
        let addr = self.read_u16(vec_addr);
        println!("Interrupt vector for {:?} is {:#06x} at {:#06x}",
                 int,
                 addr,
                 vec_addr);
        self.reg.pc = addr;
    }
}
impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{:?}\n{:#?}\n{:#?}",
               self.reg,
               self.ppu_reg,
               self.apu_reg)
    }
}

struct Reg {
    pc: u16,
    sp: u8,
    status: StatusReg,
    acc: u8,
    x: u8,
    y: u8,
}
impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "pc={:#x} sp={:#x} acc={:#x} x={:#x} y={:#x} status={:#?}",
               self.pc,
               self.sp,
               self.acc,
               self.x,
               self.y,
               self.status)
    }
}
impl Default for Reg {
    fn default() -> Reg {
        Reg {
            pc: 0,
            sp: 0xfd,
            status: StatusReg::default(),
            acc: 0,
            x: 0,
            y: 0,
        }
    }
}

#[derive(Debug)]
struct StatusReg {
    neg: bool,
    overflow: bool,
    is_brk: bool,
    decimal_mode: bool,
    irq_disabled: bool,
    zero: bool,
    carry: bool,
}
impl Default for StatusReg {
    fn default() -> StatusReg {
        StatusReg {
            neg: false,
            overflow: false,
            is_brk: false,
            decimal_mode: false,
            irq_disabled: true,
            zero: false,
            carry: false,
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
            _ => panic!("Not a valid PpuReg address: {:#x}", addr),
        }
    }

    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0 => self.ppu_ctrl1 = PpuCtrl1::from(value),
            1 => self.ppu_ctrl2 = PpuCtrl2::from(value),
            _ => panic!("Invalid PpuReg.write({:#x}, {:#x})", addr, value),
        }
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
    Brk,
    Irq,
    Nmi,
    Reset,
}
impl Interrupt {
    fn addr(&self) -> u16 {
        match *self {
            Interrupt::Brk => 0xfffe,
            Interrupt::Irq => 0xfffe,
            Interrupt::Nmi => 0xfffa,
            Interrupt::Reset => 0xfffc,
        }
    }
}

#[derive(Debug)]
enum Instr {
    Adc(AddrMode),
    And(AddrMode),
    Asl(AddrMode),
    Bcc(AddrMode),
    Bcs(AddrMode),
    Beq(AddrMode),
    Bit(AddrMode),
    Bmi(AddrMode),
    Bne(AddrMode),
    Bpl(AddrMode),
    Brk,
    Bvc(AddrMode),
    Bvs(AddrMode),
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp(AddrMode),
    Cpx(AddrMode),
    Cpy(AddrMode),
    Dec(AddrMode),
    Dex,
    Dey,
    Eor(AddrMode),
    Inc(AddrMode),
    Inx,
    Iny,
    Jmp(AddrMode),
    Jsr(AddrMode),
    Lda(AddrMode),
    Ldx(AddrMode),
    Ldy(AddrMode),
    Lsr(AddrMode),
    Nop,
    Ora(AddrMode),
    Pha,
    Php,
    Pla,
    Plp,
    Rol(AddrMode),
    Ror(AddrMode),
    Rti,
    Rts,
    Sbc(AddrMode),
    Sec,
    Sed,
    Sei,
    Slo(AddrMode),
    Sta(AddrMode),
    Stx(AddrMode),
    Sty(AddrMode),
    Stp,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
    Illegal(u8),
}
impl Instr {
    fn from(opcode: u8) -> Instr {
        match opcode {
            0x00 => Instr::Brk,
            0x01 => Instr::Ora(AddrMode::IndX),
            0x05 => Instr::Ora(AddrMode::Zero),
            0x06 => Instr::Asl(AddrMode::Zero),
            0x08 => Instr::Php,
            0x09 => Instr::Ora(AddrMode::Immediate),
            0x0a => Instr::Asl(AddrMode::Acc),
            0x0d => Instr::Ora(AddrMode::Abs),
            0x0e => Instr::Asl(AddrMode::Abs),
            0x10 => Instr::Bpl(AddrMode::Rel),
            0x11 => Instr::Ora(AddrMode::IndY),
            0x15 => Instr::Ora(AddrMode::ZeroX),
            0x16 => Instr::Asl(AddrMode::ZeroX),
            0x18 => Instr::Clc,
            0x19 => Instr::Ora(AddrMode::AbsY),
            0x1d => Instr::Ora(AddrMode::AbsX),
            0x1e => Instr::Asl(AddrMode::AbsX),
            0x20 => Instr::Jsr(AddrMode::Abs),
            0x21 => Instr::And(AddrMode::IndX),
            0x24 => Instr::Bit(AddrMode::Zero),
            0x25 => Instr::And(AddrMode::Zero),
            0x26 => Instr::Rol(AddrMode::Zero),
            0x28 => Instr::Plp,
            0x29 => Instr::And(AddrMode::Immediate),
            0x2a => Instr::Rol(AddrMode::Acc),
            0x2c => Instr::Bit(AddrMode::Abs),
            0x2d => Instr::And(AddrMode::Abs),
            0x2e => Instr::Rol(AddrMode::Abs),
            0x30 => Instr::Bmi(AddrMode::Rel),
            0x31 => Instr::And(AddrMode::IndY),
            0x35 => Instr::And(AddrMode::Zero),
            0x36 => Instr::Rol(AddrMode::Zero),
            0x38 => Instr::Sec,
            0x39 => Instr::And(AddrMode::AbsY),
            0x3d => Instr::And(AddrMode::AbsX),
            0x3e => Instr::Rol(AddrMode::AbsX),
            0x40 => Instr::Rti,
            0x41 => Instr::Eor(AddrMode::IndX),
            0x45 => Instr::Eor(AddrMode::Zero),
            0x46 => Instr::Lsr(AddrMode::Zero),
            0x48 => Instr::Pha,
            0x49 => Instr::Eor(AddrMode::Immediate),
            0x4a => Instr::Lsr(AddrMode::Acc),
            0x4c => Instr::Jmp(AddrMode::Abs),
            0x4d => Instr::Eor(AddrMode::Abs),
            0x4e => Instr::Lsr(AddrMode::Abs),
            0x50 => Instr::Bvc(AddrMode::Rel),
            0x51 => Instr::Eor(AddrMode::IndY),
            0x55 => Instr::Eor(AddrMode::ZeroX),
            0x56 => Instr::Lsr(AddrMode::ZeroX),
            0x58 => Instr::Cli,
            0x59 => Instr::Eor(AddrMode::AbsY),
            0x5d => Instr::Eor(AddrMode::AbsX),
            0x5e => Instr::Lsr(AddrMode::AbsX),
            0x60 => Instr::Rts,
            0x61 => Instr::Adc(AddrMode::IndX),
            0x65 => Instr::Adc(AddrMode::Zero),
            0x66 => Instr::Ror(AddrMode::Zero),
            0x68 => Instr::Pla,
            0x69 => Instr::Adc(AddrMode::Immediate),
            0x6a => Instr::Ror(AddrMode::Acc),
            0x6c => Instr::Jmp(AddrMode::Ind),
            0x6d => Instr::Adc(AddrMode::Abs),
            0x6e => Instr::Ror(AddrMode::Abs),
            0x70 => Instr::Bvs(AddrMode::Rel),
            0x71 => Instr::Adc(AddrMode::IndY),
            0x75 => Instr::Adc(AddrMode::ZeroX),
            0x76 => Instr::Ror(AddrMode::ZeroX),
            0x78 => Instr::Sei,
            0x79 => Instr::Adc(AddrMode::AbsY),
            0x7d => Instr::Adc(AddrMode::AbsX),
            0x7e => Instr::Ror(AddrMode::AbsX),
            0x81 => Instr::Sta(AddrMode::IndX),
            0x84 => Instr::Sty(AddrMode::Zero),
            0x85 => Instr::Sta(AddrMode::Zero),
            0x86 => Instr::Stx(AddrMode::Zero),
            0x88 => Instr::Dey,
            0x8a => Instr::Txa,
            0x8c => Instr::Sty(AddrMode::Abs),
            0x8d => Instr::Sta(AddrMode::Abs),
            0x8e => Instr::Stx(AddrMode::Abs),
            0x90 => Instr::Bcc(AddrMode::Rel),
            0x91 => Instr::Sta(AddrMode::IndY),
            0x94 => Instr::Sty(AddrMode::ZeroX),
            0x95 => Instr::Sta(AddrMode::ZeroX),
            0x96 => Instr::Stx(AddrMode::ZeroY),
            0x98 => Instr::Tya,
            0x99 => Instr::Sta(AddrMode::AbsY),
            0x9a => Instr::Txs,
            0x9d => Instr::Sta(AddrMode::AbsX),
            0xa0 => Instr::Ldy(AddrMode::Immediate),
            0xa1 => Instr::Lda(AddrMode::IndX),
            0xa2 => Instr::Ldx(AddrMode::Immediate),
            0xa4 => Instr::Ldy(AddrMode::Zero),
            0xa5 => Instr::Lda(AddrMode::Zero),
            0xa6 => Instr::Ldx(AddrMode::Zero),
            0xa8 => Instr::Tay,
            0xa9 => Instr::Lda(AddrMode::Immediate),
            0xaa => Instr::Tax,
            0xac => Instr::Ldy(AddrMode::Abs),
            0xad => Instr::Lda(AddrMode::Abs),
            0xae => Instr::Ldx(AddrMode::Abs),
            0xb0 => Instr::Bcs(AddrMode::Rel),
            0xb1 => Instr::Lda(AddrMode::IndY),
            0xb4 => Instr::Ldy(AddrMode::ZeroX),
            0xb5 => Instr::Lda(AddrMode::ZeroX),
            0xb6 => Instr::Ldx(AddrMode::ZeroX),
            0xb8 => Instr::Clv,
            0xb9 => Instr::Lda(AddrMode::AbsY),
            0xba => Instr::Tsx,
            0xbc => Instr::Ldy(AddrMode::AbsX),
            0xbd => Instr::Lda(AddrMode::AbsX),
            0xbe => Instr::Ldx(AddrMode::AbsY),
            0xc0 => Instr::Cpy(AddrMode::Immediate),
            0xc1 => Instr::Cmp(AddrMode::IndY),
            0xc4 => Instr::Cpy(AddrMode::Zero),
            0xc5 => Instr::Cmp(AddrMode::Zero),
            0xc6 => Instr::Dec(AddrMode::Zero),
            0xc8 => Instr::Iny,
            0xc9 => Instr::Cmp(AddrMode::Immediate),
            0xca => Instr::Dex,
            0xcc => Instr::Cpy(AddrMode::Abs),
            0xcd => Instr::Cmp(AddrMode::Abs),
            0xce => Instr::Dec(AddrMode::Abs),
            0xd0 => Instr::Bne(AddrMode::Rel),
            0xd1 => Instr::Cmp(AddrMode::IndY),
            0xd5 => Instr::Cmp(AddrMode::ZeroX),
            0xd6 => Instr::Dec(AddrMode::ZeroX),
            0xd8 => Instr::Cld,
            0xd9 => Instr::Cmp(AddrMode::AbsY),
            0xdd => Instr::Cmp(AddrMode::AbsX),
            0xde => Instr::Dec(AddrMode::AbsX),
            0xe0 => Instr::Cpx(AddrMode::Immediate),
            0xe1 => Instr::Sbc(AddrMode::IndX),
            0xe4 => Instr::Cpx(AddrMode::Zero),
            0xe5 => Instr::Sbc(AddrMode::Zero),
            0xe6 => Instr::Inc(AddrMode::Zero),
            0xe8 => Instr::Inx,
            0xe9 => Instr::Sbc(AddrMode::Immediate),
            0xea => Instr::Nop,
            0xec => Instr::Cpx(AddrMode::Abs),
            0xed => Instr::Sbc(AddrMode::Abs),
            0xee => Instr::Inc(AddrMode::Abs),
            0xf0 => Instr::Beq(AddrMode::Rel),
            0xf1 => Instr::Sbc(AddrMode::IndY),
            0xf5 => Instr::Sbc(AddrMode::ZeroX),
            0xf6 => Instr::Inc(AddrMode::ZeroX),
            0xf8 => Instr::Sed,
            0xf9 => Instr::Sbc(AddrMode::AbsY),
            0xfd => Instr::Sbc(AddrMode::AbsX),
            0xfe => Instr::Inc(AddrMode::AbsX),
            other => Instr::Illegal(other),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum AddrMode {
    Abs,
    AbsX,
    AbsY,
    Acc,
    Immediate,
    Impl,
    Ind,
    IndX,
    IndY,
    Rel,
    Zero,
    ZeroX,
    ZeroY,
}
impl AddrMode {
    fn payload_len(self) -> u16 {
        match self {
            AddrMode::Abs => 2,
            AddrMode::AbsX => 2,
            AddrMode::AbsY => 2,
            AddrMode::Acc => 0,
            AddrMode::Immediate => 1,
            AddrMode::Impl => 0,
            AddrMode::Ind => 1,
            AddrMode::IndX => 1,
            AddrMode::IndY => 1,
            AddrMode::Rel => 1,
            AddrMode::Zero => 1,
            AddrMode::ZeroX => 1,
            AddrMode::ZeroY => 1,
        }
    }
}
