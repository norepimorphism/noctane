use crate::{bus, mem, mmu};

pub type Queue = const_queue::ConstQueue::<Kind, 2>;

impl From<mmu::Error> for Kind {
    fn from(e: mmu::Error) -> Self {
        match e {
            mmu::Error::Memory(e) => e.into(),
        }
    }
}

impl From<mem::Error> for Kind {
    fn from(e: mem::Error) -> Self {
        match e {
            mem::Error::Bus(e) => e.into(),
        }
    }
}

impl From<bus::Error> for Kind {
    fn from(e: bus::Error) -> Self {
        match e {
            // TODO: Disambiguate between address load and store.
            bus::Error::UnmappedAddress(_) => Self::AddressLoad,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Kind {
    Interrupt = 0,
    TlbModification = 1,
    TlbLoad = 2,
    TlbStore = 3,
    AddressLoad = 4,
    AddressStore = 5,
    BusFetch = 6,
    BusLoad = 7,
    Syscall = 8,
    Breakpoint = 9,
    ReservedInstr = 10,
    CopUnusable = 11,
    IntegerOverflow = 12,
}

impl Kind {
    pub fn vector(self) -> u32 {
        // TODO
        0x8000_0080
    }
}
