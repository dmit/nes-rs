use std::cell::RefCell;
use std::fmt;
use std::io::{Read, Write};
use std::rc::Rc;

use apu::ApuReg;
use cpu_instr::{Cycles, Instr, Mode, Op};
use ppu::PpuReg;
use rom::Rom;

pub struct Cpu {
    reg: Reg,
    ram: [u8; 0x800],
    ppu_reg: Rc<RefCell<PpuReg>>,
    apu_reg: Rc<RefCell<ApuReg>>,
    sram: [u8; 0x2000],
    prg_rom: [u8; 0x8000],
}
impl Cpu {
    pub fn new(ppu_reg: Rc<RefCell<PpuReg>>, apu_reg: Rc<RefCell<ApuReg>>, rom: Rom) -> Cpu {
        let mut prg_rom = [0u8; 0x8000];
        for (i, b) in rom.prg_rom
                         .iter()
                         .cycle()
                         .take(0x8000)
                         .enumerate() {
            prg_rom[i] = *b;
        }

        Cpu {
            reg: Reg::default(),
            ram: [0; 0x800],
            ppu_reg: ppu_reg,
            apu_reg: apu_reg,
            sram: [0; 0x2000],
            prg_rom: prg_rom,
        }
    }

    fn read_u8(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x07ff => self.ram[addr as usize],
            0x0800...0x1fff => self.ram[(addr % 0x800) as usize],
            0x2000...0x2007 => self.ppu_reg.borrow().read(addr - 0x2000),
            0x2008...0x3fff => self.ppu_reg.borrow().read((addr - 0x2008) % 8),
            0x4000...0x401f => self.apu_reg.borrow().read(addr - 0x4000),
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
            0x2000...0x2007 => self.ppu_reg.borrow_mut().write(addr - 0x2000, value),
            0x2008...0x3fff => self.ppu_reg.borrow_mut().write((addr - 0x2008) % 8, value),
            0x4000...0x401f => self.apu_reg.borrow_mut().write(addr - 0x4000, value),
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

    pub fn exec(&mut self) -> u8 {
        let addr = self.reg.pc;
        let opcode = self.read_u8(addr);
        let instr = Instr::from(opcode);
        let payload: String = if instr.1.payload_len() == 0 {
            String::from("")
        } else {
            let mut hex = String::with_capacity(2 + instr.1.payload_len() as usize * 2);
            hex.push_str("0x");
            for i in (0..instr.1.payload_len()).rev() {
                let byte = self.read_u8(addr + 1 + i);
                hex.push_str(&format!("{:02x}", byte));
            }
            hex
        };
        println!("{:#06x} {:#04x} {:?} {}", addr, opcode, instr, payload);

        self.reg.pc = self.reg.pc.wrapping_add(1);

        match instr {
            Instr(Op::And, m, c) => {
                let mem = self.payload_u8(m);
                let val = self.reg.acc & mem;

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.acc = val;
            }
            Instr(Op::Asl, m, c) => {
                let addr = self.payload_u16(m);
                let old_val = self.read_u8(addr);
                let new_val = old_val << 1;

                self.reg.status.carry = old_val & 0b1000_0000 != 0;
                self.reg.status.neg = new_val & 0b1000_0000 != 0;
                self.reg.status.zero = new_val == 0;

                self.write_u8(addr, new_val);
            }
            Instr(Op::Bcs, m, c) => {
                let addr = self.payload_u16(m);
                if self.reg.status.carry {
                    self.reg.pc = addr;
                }
            }
            Instr(Op::Beq, m, c) => {
                let addr = self.payload_u16(m);
                if self.reg.status.zero {
                    self.reg.pc = addr;
                }
            }
            Instr(Op::Bit, m, c) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.overflow = val & 0b0100_0000 != 0;
                self.reg.status.zero = val & self.reg.acc == 0;
            }
            Instr(Op::Bmi, m, c) => {
                let addr = self.payload_u16(m);
                if self.reg.status.neg {
                    self.reg.pc = addr;
                }
            }
            Instr(Op::Bne, m, c) => {
                let addr = self.payload_u16(m);
                if !self.reg.status.zero {
                    self.reg.pc = addr;
                }
            }
            Instr(Op::Bpl, m, c) => {
                let addr = self.payload_addr(m);
                if !self.reg.status.neg {
                    self.reg.pc = addr;
                }
            }
            Instr(Op::Brk, m, c) => self.interrupt(Interrupt::Brk),
            Instr(Op::Cld, m, c) => self.reg.status.decimal_mode = false,
            Instr(Op::Cmp, m, c) => {
                let mem = self.payload_u8(m);
                let val = self.reg.acc - mem;

                self.reg.status.carry = self.reg.acc >= mem;
                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;
            }
            Instr(Op::Dec, m, c) => {
                let addr = self.payload_u16(m);
                let val = self.read_u8(addr).wrapping_sub(1);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.write_u8(addr, val);
            }
            Instr(Op::Dex, m, c) => {
                self.reg.x = self.reg.x.wrapping_sub(1);
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;
            }
            Instr(Op::Dey, m, c) => {
                self.reg.y = self.reg.y.wrapping_sub(1);
                self.reg.status.neg = self.reg.y & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.y == 0;
            }
            Instr(Op::Inc, m, c) => {
                let addr = self.payload_u16(m);
                let val = self.read_u8(addr).wrapping_add(1);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.write_u8(addr, val);
            }
            Instr(Op::Inx, m, c) => {
                self.reg.x = self.reg.x.wrapping_add(1);
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;
            }
            Instr(Op::Iny, m, c) => {
                self.reg.y = self.reg.y.wrapping_add(1);
                self.reg.status.neg = self.reg.y & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.y == 0;
            }
            Instr(Op::Lda, m, c) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.acc = val;
            }
            Instr(Op::Ldx, m, c) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.x = val;
            }
            Instr(Op::Ldy, m, c) => {
                let val = self.payload_u8(m);

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;

                self.reg.y = val;
            }
            Instr(Op::Ora, m, c) => {
                let mem = self.payload_u8(m);
                let val = self.reg.acc | mem;

                self.reg.status.neg = val & 0b1000_0000 != 0;
                self.reg.status.zero = val == 0;
            }
            Instr(Op::Rts, m, c) => self.reg.pc = self.pop_u16().wrapping_add(1),
            Instr(Op::Sei, m, c) => self.reg.status.irq_disabled = true,
            Instr(Op::Sta, m, c) => {
                let addr = self.payload_addr(m);
                let val = self.reg.acc;
                self.write_u8(addr, val);
            }
            Instr(Op::Stx, m, c) => {
                let addr = self.payload_addr(m);
                let val = self.reg.x;
                self.write_u8(addr, val);
            }
            Instr(Op::Sty, m, c) => {
                let addr = self.payload_addr(m);
                let val = self.reg.y;
                self.write_u8(addr, val);
            }
            Instr(Op::Txa, m, c) => {
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;

                self.reg.acc = self.reg.x;
            }
            Instr(Op::Txs, m, c) => {
                self.reg.status.neg = self.reg.x & 0b1000_0000 != 0;
                self.reg.status.zero = self.reg.x == 0;

                self.reg.sp = self.reg.x;
            }
            Instr(Op::Tya, m, c) => {
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

        match instr.2 {
            Cycles::A(c) => c,
            Cycles::B(c) => c,
            Cycles::C(c) => c,
        }
    }

    fn payload_addr(&mut self, mode: Mode) -> u16 {
        let addr = match mode {
            Mode::Abs => self.read_u16(self.reg.pc),
            Mode::AbsX => {
                let offset = self.read_u8(self.reg.pc);
                (self.reg.x as u16).wrapping_add(offset as u16)
            }
            Mode::AbsY => {
                let offset = self.read_u8(self.reg.pc);
                (self.reg.y as u16).wrapping_add(offset as u16)
            }
            Mode::Imm => self.reg.pc,
            Mode::Ind => {
                let addr = self.read_u16(self.reg.pc);
                self.read_u16(addr)
            }
            Mode::IndX => {
                let zero_offset = self.read_u8(self.reg.pc);
                let zero_addr = self.reg.x as u16 + zero_offset as u16;
                self.read_u16(zero_addr)
            }
            Mode::IndY => {
                let zero_offset = self.read_u8(self.reg.pc);
                let zero_addr = self.read_u16(zero_offset as u16);
                zero_addr + self.reg.y as u16
            }
            Mode::Rel => {
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
            Mode::Zero => self.read_u8(self.reg.pc) as u16,
            Mode::ZeroX => {
                let offset = self.read_u8(self.reg.pc);
                self.reg.x.wrapping_add(offset) as u16
            }
            Mode::ZeroY => {
                let offset = self.read_u8(self.reg.pc);
                self.reg.y.wrapping_add(offset) as u16
            }
            _ => panic!("Unsupported address mode: {:?}", mode),
        };

        self.reg.pc = self.reg.pc.wrapping_add(mode.payload_len());

        addr
    }

    fn payload_u8(&mut self, mode: Mode) -> u8 {
        let addr = self.payload_addr(mode);
        self.read_u8(addr)
    }

    fn payload_u16(&mut self, mode: Mode) -> u16 {
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
        write!(f, "{:?}", self.reg)
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
