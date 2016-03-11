extern crate byteorder;

use std::env;
use std::fs::File;
use std::io::{Cursor, Read};

use byteorder::{BigEndian, ReadBytesExt};

fn main() {
    let rom_path = env::args().nth(1).expect("Path to rom");
    let mut rom_file = File::open(rom_path).expect("Rom file not found");
    let mut rom_bytes: Vec<u8> = Vec::new();
    rom_file.read_to_end(&mut rom_bytes);
    let rom = Rom::from(&rom_bytes);

    println!("{:?}", rom);
}

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
#[derive(Debug)]
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
