use byteorder::{LittleEndian, ByteOrder};
use std::fmt;

pub struct Instr(pub Op, pub Mode, pub Cycles);
impl Instr {
    pub fn from(code: &[u8]) -> Instr {
        let opcode = code[0];
        let oper = &code[1..];
        match opcode {
            0x00 => Instr(Op::Brk, Mode::Impl, Cycles::A(7)),
            0x01 => Instr(Op::Ora, Mode::ind_x(oper), Cycles::A(6)),
            0x05 => Instr(Op::Ora, Mode::zero(oper), Cycles::A(3)),
            0x06 => Instr(Op::Asl, Mode::zero(oper), Cycles::A(5)),
            0x08 => Instr(Op::Php, Mode::Impl, Cycles::A(3)),
            0x09 => Instr(Op::Ora, Mode::imm(oper), Cycles::A(2)),
            0x0a => Instr(Op::Asl, Mode::Acc, Cycles::A(2)),
            0x0d => Instr(Op::Ora, Mode::abs(oper), Cycles::A(4)),
            0x0e => Instr(Op::Asl, Mode::abs(oper), Cycles::A(6)),
            0x10 => Instr(Op::Bpl, Mode::rel(oper), Cycles::C(2)),
            0x11 => Instr(Op::Ora, Mode::ind_y(oper), Cycles::B(5)),
            0x15 => Instr(Op::Ora, Mode::zero_x(oper), Cycles::A(4)),
            0x16 => Instr(Op::Asl, Mode::zero_x(oper), Cycles::A(6)),
            0x18 => Instr(Op::Clc, Mode::Impl, Cycles::A(2)),
            0x19 => Instr(Op::Ora, Mode::abs_y(oper), Cycles::B(4)),
            0x1d => Instr(Op::Ora, Mode::abs_x(oper), Cycles::B(4)),
            0x1e => Instr(Op::Asl, Mode::abs_x(oper), Cycles::A(7)),
            0x20 => Instr(Op::Jsr, Mode::abs(oper), Cycles::A(6)),
            0x21 => Instr(Op::And, Mode::ind_x(oper), Cycles::A(6)),
            0x24 => Instr(Op::Bit, Mode::zero(oper), Cycles::A(3)),
            0x25 => Instr(Op::And, Mode::zero(oper), Cycles::A(3)),
            0x26 => Instr(Op::Rol, Mode::zero(oper), Cycles::A(5)),
            0x28 => Instr(Op::Plp, Mode::Impl, Cycles::A(4)),
            0x29 => Instr(Op::And, Mode::imm(oper), Cycles::A(2)),
            0x2a => Instr(Op::Rol, Mode::Acc, Cycles::A(2)),
            0x2c => Instr(Op::Bit, Mode::abs(oper), Cycles::A(4)),
            0x2d => Instr(Op::And, Mode::abs(oper), Cycles::A(4)),
            0x2e => Instr(Op::Rol, Mode::abs(oper), Cycles::A(6)),
            0x30 => Instr(Op::Bmi, Mode::rel(oper), Cycles::C(2)),
            0x31 => Instr(Op::And, Mode::ind_y(oper), Cycles::B(5)),
            0x35 => Instr(Op::And, Mode::zero_x(oper), Cycles::A(4)),
            0x36 => Instr(Op::Rol, Mode::zero(oper), Cycles::A(5)),
            0x38 => Instr(Op::Sec, Mode::Impl, Cycles::A(2)),
            0x39 => Instr(Op::And, Mode::abs_y(oper), Cycles::B(4)),
            0x3d => Instr(Op::And, Mode::abs_x(oper), Cycles::B(4)),
            0x3e => Instr(Op::Rol, Mode::abs_x(oper), Cycles::A(7)),
            0x40 => Instr(Op::Rti, Mode::Impl, Cycles::A(6)),
            0x41 => Instr(Op::Eor, Mode::ind_x(oper), Cycles::A(6)),
            0x45 => Instr(Op::Eor, Mode::zero(oper), Cycles::A(3)),
            0x46 => Instr(Op::Lsr, Mode::zero(oper), Cycles::A(5)),
            0x48 => Instr(Op::Pha, Mode::Impl, Cycles::A(3)),
            0x49 => Instr(Op::Eor, Mode::imm(oper), Cycles::A(2)),
            0x4a => Instr(Op::Lsr, Mode::Acc, Cycles::A(2)),
            0x4c => Instr(Op::Jmp, Mode::abs(oper), Cycles::A(3)),
            0x4d => Instr(Op::Eor, Mode::abs(oper), Cycles::A(4)),
            0x4e => Instr(Op::Lsr, Mode::abs(oper), Cycles::A(6)),
            0x50 => Instr(Op::Bvc, Mode::rel(oper), Cycles::C(2)),
            0x51 => Instr(Op::Eor, Mode::ind_y(oper), Cycles::B(5)),
            0x55 => Instr(Op::Eor, Mode::zero_x(oper), Cycles::A(4)),
            0x56 => Instr(Op::Lsr, Mode::zero_x(oper), Cycles::A(6)),
            0x58 => Instr(Op::Cli, Mode::Impl, Cycles::A(2)),
            0x59 => Instr(Op::Eor, Mode::abs_y(oper), Cycles::B(4)),
            0x5d => Instr(Op::Eor, Mode::abs_x(oper), Cycles::B(4)),
            0x5e => Instr(Op::Lsr, Mode::abs_x(oper), Cycles::A(7)),
            0x60 => Instr(Op::Rts, Mode::Impl, Cycles::A(6)),
            0x61 => Instr(Op::Adc, Mode::ind_x(oper), Cycles::A(6)),
            0x65 => Instr(Op::Adc, Mode::zero(oper), Cycles::A(3)),
            0x66 => Instr(Op::Ror, Mode::zero(oper), Cycles::A(5)),
            0x68 => Instr(Op::Pla, Mode::Impl, Cycles::A(4)),
            0x69 => Instr(Op::Adc, Mode::imm(oper), Cycles::A(2)),
            0x6a => Instr(Op::Ror, Mode::Acc, Cycles::A(2)),
            0x6c => Instr(Op::Jmp, Mode::ind(oper), Cycles::A(5)),
            0x6d => Instr(Op::Adc, Mode::abs(oper), Cycles::A(4)),
            0x6e => Instr(Op::Ror, Mode::abs(oper), Cycles::A(6)),
            0x70 => Instr(Op::Bvs, Mode::rel(oper), Cycles::C(2)),
            0x71 => Instr(Op::Adc, Mode::ind_y(oper), Cycles::B(5)),
            0x75 => Instr(Op::Adc, Mode::zero_x(oper), Cycles::A(4)),
            0x76 => Instr(Op::Ror, Mode::zero_x(oper), Cycles::A(6)),
            0x78 => Instr(Op::Sei, Mode::Impl, Cycles::A(2)),
            0x79 => Instr(Op::Adc, Mode::abs_y(oper), Cycles::B(4)),
            0x7d => Instr(Op::Adc, Mode::abs_x(oper), Cycles::B(4)),
            0x7e => Instr(Op::Ror, Mode::abs_x(oper), Cycles::A(7)),
            0x81 => Instr(Op::Sta, Mode::ind_x(oper), Cycles::A(6)),
            0x84 => Instr(Op::Sty, Mode::zero(oper), Cycles::A(3)),
            0x85 => Instr(Op::Sta, Mode::zero(oper), Cycles::A(3)),
            0x86 => Instr(Op::Stx, Mode::zero(oper), Cycles::A(3)),
            0x88 => Instr(Op::Dey, Mode::Impl, Cycles::A(2)),
            0x8a => Instr(Op::Txa, Mode::Impl, Cycles::A(2)),
            0x8c => Instr(Op::Sty, Mode::abs(oper), Cycles::A(4)),
            0x8d => Instr(Op::Sta, Mode::abs(oper), Cycles::A(4)),
            0x8e => Instr(Op::Stx, Mode::abs(oper), Cycles::A(4)),
            0x90 => Instr(Op::Bcc, Mode::rel(oper), Cycles::C(2)),
            0x91 => Instr(Op::Sta, Mode::ind_y(oper), Cycles::A(6)),
            0x94 => Instr(Op::Sty, Mode::zero_x(oper), Cycles::A(4)),
            0x95 => Instr(Op::Sta, Mode::zero_x(oper), Cycles::A(4)),
            0x96 => Instr(Op::Stx, Mode::zero_y(oper), Cycles::A(4)),
            0x98 => Instr(Op::Tya, Mode::Impl, Cycles::A(2)),
            0x99 => Instr(Op::Sta, Mode::abs_y(oper), Cycles::A(5)),
            0x9a => Instr(Op::Txs, Mode::Impl, Cycles::A(2)),
            0x9d => Instr(Op::Sta, Mode::abs_x(oper), Cycles::A(5)),
            0xa0 => Instr(Op::Ldy, Mode::imm(oper), Cycles::A(2)),
            0xa1 => Instr(Op::Lda, Mode::ind_x(oper), Cycles::A(6)),
            0xa2 => Instr(Op::Ldx, Mode::imm(oper), Cycles::A(2)),
            0xa4 => Instr(Op::Ldy, Mode::zero(oper), Cycles::A(3)),
            0xa5 => Instr(Op::Lda, Mode::zero(oper), Cycles::A(3)),
            0xa6 => Instr(Op::Ldx, Mode::zero(oper), Cycles::A(3)),
            0xa8 => Instr(Op::Tay, Mode::Impl, Cycles::A(2)),
            0xa9 => Instr(Op::Lda, Mode::imm(oper), Cycles::A(2)),
            0xaa => Instr(Op::Tax, Mode::Impl, Cycles::A(2)),
            0xac => Instr(Op::Ldy, Mode::abs(oper), Cycles::A(4)),
            0xad => Instr(Op::Lda, Mode::abs(oper), Cycles::A(4)),
            0xae => Instr(Op::Ldx, Mode::abs(oper), Cycles::A(4)),
            0xb0 => Instr(Op::Bcs, Mode::rel(oper), Cycles::C(2)),
            0xb1 => Instr(Op::Lda, Mode::ind_y(oper), Cycles::B(5)),
            0xb4 => Instr(Op::Ldy, Mode::zero_x(oper), Cycles::A(4)),
            0xb5 => Instr(Op::Lda, Mode::zero_x(oper), Cycles::A(4)),
            0xb6 => Instr(Op::Ldx, Mode::zero_y(oper), Cycles::A(4)),
            0xb8 => Instr(Op::Clv, Mode::Impl, Cycles::A(2)),
            0xb9 => Instr(Op::Lda, Mode::abs_y(oper), Cycles::B(4)),
            0xba => Instr(Op::Tsx, Mode::Impl, Cycles::A(2)),
            0xbc => Instr(Op::Ldy, Mode::abs_x(oper), Cycles::B(4)),
            0xbd => Instr(Op::Lda, Mode::abs_x(oper), Cycles::B(4)),
            0xbe => Instr(Op::Ldx, Mode::abs_y(oper), Cycles::B(4)),
            0xc0 => Instr(Op::Cpy, Mode::imm(oper), Cycles::A(2)),
            0xc1 => Instr(Op::Cmp, Mode::ind_x(oper), Cycles::A(6)),
            0xc4 => Instr(Op::Cpy, Mode::zero(oper), Cycles::A(3)),
            0xc5 => Instr(Op::Cmp, Mode::zero(oper), Cycles::A(3)),
            0xc6 => Instr(Op::Dec, Mode::zero(oper), Cycles::A(5)),
            0xc8 => Instr(Op::Iny, Mode::Impl, Cycles::A(2)),
            0xc9 => Instr(Op::Cmp, Mode::imm(oper), Cycles::A(2)),
            0xca => Instr(Op::Dex, Mode::Impl, Cycles::A(2)),
            0xcc => Instr(Op::Cpy, Mode::abs(oper), Cycles::A(4)),
            0xcd => Instr(Op::Cmp, Mode::abs(oper), Cycles::A(4)),
            0xce => Instr(Op::Dec, Mode::abs(oper), Cycles::A(3)),
            0xd0 => Instr(Op::Bne, Mode::rel(oper), Cycles::C(2)),
            0xd1 => Instr(Op::Cmp, Mode::ind_y(oper), Cycles::B(5)),
            0xd5 => Instr(Op::Cmp, Mode::zero_x(oper), Cycles::A(4)),
            0xd6 => Instr(Op::Dec, Mode::zero_x(oper), Cycles::A(6)),
            0xd8 => Instr(Op::Cld, Mode::Impl, Cycles::A(2)),
            0xd9 => Instr(Op::Cmp, Mode::abs_y(oper), Cycles::B(4)),
            0xdd => Instr(Op::Cmp, Mode::abs_x(oper), Cycles::B(4)),
            0xde => Instr(Op::Dec, Mode::abs_x(oper), Cycles::A(7)),
            0xe0 => Instr(Op::Cpx, Mode::imm(oper), Cycles::A(2)),
            0xe1 => Instr(Op::Sbc, Mode::ind_x(oper), Cycles::A(6)),
            0xe4 => Instr(Op::Cpx, Mode::zero(oper), Cycles::A(3)),
            0xe5 => Instr(Op::Sbc, Mode::zero(oper), Cycles::A(3)),
            0xe6 => Instr(Op::Inc, Mode::zero(oper), Cycles::A(5)),
            0xe8 => Instr(Op::Inx, Mode::Impl, Cycles::A(2)),
            0xe9 => Instr(Op::Sbc, Mode::imm(oper), Cycles::A(2)),
            0xea => Instr(Op::Nop, Mode::Impl, Cycles::A(2)),
            0xec => Instr(Op::Cpx, Mode::abs(oper), Cycles::A(4)),
            0xed => Instr(Op::Sbc, Mode::abs(oper), Cycles::A(4)),
            0xee => Instr(Op::Inc, Mode::abs(oper), Cycles::A(6)),
            0xf0 => Instr(Op::Beq, Mode::rel(oper), Cycles::C(2)),
            0xf1 => Instr(Op::Sbc, Mode::ind_y(oper), Cycles::B(5)),
            0xf5 => Instr(Op::Sbc, Mode::zero_x(oper), Cycles::A(4)),
            0xf6 => Instr(Op::Inc, Mode::zero_x(oper), Cycles::A(6)),
            0xf8 => Instr(Op::Sed, Mode::Impl, Cycles::A(2)),
            0xf9 => Instr(Op::Sbc, Mode::abs_y(oper), Cycles::B(4)),
            0xfd => Instr(Op::Sbc, Mode::abs_x(oper), Cycles::B(4)),
            0xfe => Instr(Op::Inc, Mode::abs_x(oper), Cycles::A(7)),
            other => Instr(Op::Illegal(other), Mode::Impl, Cycles::A(0)),
        }
    }
}
impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = format!("{:?}", self.0).to_lowercase();
        match self.1 {
            Mode::Abs(p) => write!(f, "{} ${:04x}", op, p),
            Mode::AbsX(p) => write!(f, "{} ${:04x},x", op, p),
            Mode::AbsY(p) => write!(f, "{} ${:04x},y", op, p),
            Mode::Acc => write!(f, "{}", op),
            Mode::Imm(p) => write!(f, "{} #${:02x}", op, p),
            Mode::Impl => write!(f, "{}", op),
            Mode::Ind(p) => write!(f, "{} (${:04x})", op, p),
            Mode::IndX(p) => write!(f, "{} (${:02x},x)", op, p),
            Mode::IndY(p) => write!(f, "{} (${:02x}),y", op, p),
            Mode::Rel(p) => write!(f, "{} ${:02x}", op, p),
            Mode::Zero(p) => write!(f, "{} ${:02x}", op, p),
            Mode::ZeroX(p) => write!(f, "{} ${:02x},x", op, p),
            Mode::ZeroY(p) => write!(f, "{} ${:02x},y", op, p),
        }
    }
}

#[derive(Debug)]
pub enum Op {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Slo,
    Sta,
    Stx,
    Sty,
    Stp,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
    Illegal(u8),
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Abs(u16),
    AbsX(u16),
    AbsY(u16),
    Acc,
    Imm(u8),
    Impl,
    Ind(u16),
    IndX(u8),
    IndY(u8),
    Rel(i8),
    Zero(u8),
    ZeroX(u8),
    ZeroY(u8),
}
impl Mode {
    pub fn payload_len(self) -> u16 {
        match self {
            Mode::Abs(_) => 2,
            Mode::AbsX(_) => 2,
            Mode::AbsY(_) => 2,
            Mode::Acc => 0,
            Mode::Imm(_) => 1,
            Mode::Impl => 0,
            Mode::Ind(_) => 2,
            Mode::IndX(_) => 1,
            Mode::IndY(_) => 1,
            Mode::Rel(_) => 1,
            Mode::Zero(_) => 1,
            Mode::ZeroX(_) => 1,
            Mode::ZeroY(_) => 1,
        }
    }

    fn abs(oper: &[u8]) -> Mode {
        Mode::Abs(LittleEndian::read_u16(oper))
    }
    fn abs_x(oper: &[u8]) -> Mode {
        Mode::AbsX(LittleEndian::read_u16(oper))
    }
    fn abs_y(oper: &[u8]) -> Mode {
        Mode::AbsY(LittleEndian::read_u16(oper))
    }
    fn imm(oper: &[u8]) -> Mode {
        Mode::Imm(oper[0])
    }
    fn ind(oper: &[u8]) -> Mode {
        Mode::Ind(LittleEndian::read_u16(oper))
    }
    fn ind_x(oper: &[u8]) -> Mode {
        Mode::IndX(oper[0])
    }
    fn ind_y(oper: &[u8]) -> Mode {
        Mode::IndY(oper[0])
    }
    fn rel(oper: &[u8]) -> Mode {
        Mode::Rel(oper[0] as i8)
    }
    fn zero(oper: &[u8]) -> Mode {
        Mode::Zero(oper[0])
    }
    fn zero_x(oper: &[u8]) -> Mode {
        Mode::ZeroX(oper[0])
    }
    fn zero_y(oper: &[u8]) -> Mode {
        Mode::ZeroY(oper[0])
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Cycles {
    A(u8),
    B(u8), // +1 if page boundary crossed
    C(u8), // +1 if branch on same page; +2 if branch on different page
}
