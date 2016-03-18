#[derive(Debug, Default)]
pub struct ApuReg {
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
    pub fn read(&self, addr: u16) -> u8 {
        panic!("ApuRef read()");
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        panic!("PpuReg write()");
    }
}

#[derive(Debug, Default)]
struct SpriteDma;

#[derive(Debug, Default)]
struct VertClockSignal;
