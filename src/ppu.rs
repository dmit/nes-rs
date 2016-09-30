use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub struct Ppu {
    pub reg: Rc<RefCell<PpuReg>>,
    patterns0: PatternTable,
    patterns1: PatternTable,
    names0: NameTable,
    attrs0: AttrTable,
    names1: NameTable,
    attrs1: AttrTable,
    names2: NameTable,
    attrs2: AttrTable,
    names3: NameTable,
    attrs3: AttrTable,
    image_palette: Palette,
    sprite_palette: Palette,
}
impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            reg: Rc::new(RefCell::new(PpuReg::default())),
            patterns0: PatternTable::default(),
            patterns1: PatternTable::default(),
            names0: NameTable::default(),
            attrs0: AttrTable::default(),
            names1: NameTable::default(),
            attrs1: AttrTable::default(),
            names2: NameTable::default(),
            attrs2: AttrTable::default(),
            names3: NameTable::default(),
            attrs3: AttrTable::default(),
            image_palette: Palette::default(),
            sprite_palette: Palette::default(),
        }
    }

    pub fn exec(&mut self) {
        // TODO
    }
}
impl fmt::Debug for Ppu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.reg.borrow())
    }
}

#[derive(Clone, Copy, Default)]
struct Tile(u64, u64);

struct PatternTable {
    tiles: [Tile; 256],
}
impl Default for PatternTable {
    fn default() -> PatternTable {
        PatternTable { tiles: [Tile::default(); 256] }
    }
}

#[derive(Default)]
struct NameTable;

#[derive(Default)]
struct AttrTable;

#[derive(Default)]
struct Palette(u64, u64);

#[derive(Clone, Copy, Debug, Default)]
pub struct PpuReg {
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
    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            2 => self.ppu_status.into(),
            7 => self.vram_io,
            _ => panic!("Not a valid PpuReg address: {:#x}", addr),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0 => self.ppu_ctrl1 = PpuCtrl1::from(value),
            1 => self.ppu_ctrl2 = PpuCtrl2::from(value),
            _ => panic!("Invalid PpuReg.write({:#x}, {:#x})", addr, value),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
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

#[derive(Clone, Copy, Debug, Default)]
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

#[derive(Clone, Copy, Debug, Default)]
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
