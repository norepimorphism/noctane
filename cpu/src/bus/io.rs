use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

pub struct Io<'a> {
    pub gpu: &'a mut Gpu,
}

impl Io<'_> {
    pub fn read_32(&self, addr: u32) -> u32 {

    }

    pub fn write_32(&mut self, addr: u32, value: u32) {
        REGISTERS[LUT[addr].idx].write_32(self, addr, value);
    }
}

struct Register {
    read_8: fn(&Self, &Io, usize) -> u8,
    read_16: fn(&Self, &Io, usize) -> u16,
    read_32: fn(&Self, &Io) -> u32,
    write_8: fn(&Self, &mut Io, usize, u8),
    write_16: fn(&Self, &mut Io, usize, u16),
    write_32: fn(&Self, &mut Io, u32),
}

struct LutEntry {
    idx: usize,
    start_offset: usize,
}

gen_cpu_bus_io!(
    {
        name: SPU_DELAY,
        read_32: |this, io| 0,
        write_32: |this, io, value| {},
    }
);
