use crate::Instr;
use super::Cpu;

/// A 5-stage instruction pipeline.
///
/// A defining feature of the MIPS architecture is pipelined instruction execution. 5 instructions
/// may be queued concurrently, each in one of the 5 pipestages:
///
/// 1. Instruction Fetch (IF)
/// 2. Read and Decode (RD)
/// 3. Arithmetic/Logic Unit (ALU)
/// 4. Memory access (MEM)
/// 5. Write Back (WB)
#[derive(Debug, Default)]
pub struct Pipeline {
    if_: Option<Instr>,
    rd: Option<rd::Instr>,
    alu: Option<alu::Instr>,
    mem: Option<mem::Instr>,
}

impl Pipeline {
    /// Processes the current stage, and then advances to the next stage, of each queued
    /// instruction.
    pub fn advance(
        &mut self,
        cpu: &mut Cpu,
        fetch_instr: impl FnOnce() -> Instr,
    ) {
        if let Some(instr) = self.mem.as_mut() {
            instr.write_back();
        }
        if let Some(instr) = self.alu {
            self.mem = Some(instr.access_mem());
        }
        if let Some(instr) = self.rd {
            self.alu = Some(instr.operate());
        }
        if let Some(instr) = self.if_ {
            self.rd = Some(instr.read_reg(cpu.reg()));
        }
        self.if_ = Some(fetch_instr());
    }
}

macro_rules! _def_stage_mod {
    (
        mod: $mod:ident,
        $(
            advance_name: $advance_name:ident() $(-> $next_mod:ident)?,
            instrs: [
                $(
                    $name:ident
                    $advance:expr ;
                    $(
                        $field_name:ident
                        $field_ty:ty
                    )*
                );*
            ]
            $(
                ,
                custom: { $($custom:item)* }
            )? $(,)?
        )?
    ) => {
        pub mod $mod {
            #[derive(Debug)]
            pub enum Instr {
                $(
                    $(
                        $name($name),
                    )*
                )?
            }

            $(
                impl Instr {
                    pub fn $advance_name(self) $(-> super::$next_mod::Instr)? {
                        match self {
                            $(
                                Self::$name(it) => $advance(it),
                            )*
                        }
                    }
                }

                $(
                    #[derive(Clone, Copy, Debug)]
                    pub struct $name {
                        $(
                            pub $field_name: $field_ty,
                        )*
                    }
                )*

                $(
                    $(
                        $custom
                    )*
                )?
            )?
        }
    };
}

macro_rules! def_stages {
    (
        $(
            $name:ident {
                rd: { $( $rd_field_name:ident : $rd_field_ty:ty ),* $(,)? } => $rd:expr,
                alu: { $( $alu_field_name:ident : $alu_field_ty:ty ),* $(,)? } => $alu:expr,
                mem: { $( $mem_field_name:ident : $mem_field_ty:ty ),* $(,)? } => $mem:expr $(,)?
            }
        ),* $(,)?
    ) => {
        impl crate::Instr {
            pub fn read_reg(self, reg: &super::reg::File) -> rd::Instr {
                match self {
                    $(
                        Self::$name(it) => rd::Instr::$name(rd::$name { inner: it.read_reg(reg)}),
                    )*
                }
            }
        }

        impl crate::instr::IType {
            pub fn read_reg(self, reg: &super::reg::File) -> rd::IType {
                rd::IType {
                    rs_val: reg.gpr(self.rs.into()),
                    rt_val: reg.gpr(self.rt.into()),
                    imm: self.imm,
                }
            }
        }

        impl crate::instr::JType {
            pub fn read_reg(self, _: &super::reg::File) -> rd::JType {
                rd::JType { target: self.target }
            }
        }

        impl crate::instr::RType {
            pub fn read_reg(self, reg: &super::reg::File) -> rd::RType {
                rd::RType {
                    rs_val: reg.gpr(self.rs.into()),
                    rt_val: reg.gpr(self.rt.into()),
                    rd: self.rd,
                    shamt: self.shamt,
                    funct: self.funct,
                }
            }
        }

        _def_stage_mod! {
            mod: rd,
            advance_name: operate() -> alu,
            instrs: [$($name $rd; $($rd_field_name $rd_field_ty)*);*],
            custom: {
                #[derive(Clone, Copy, Debug)]
                pub struct IType {
                    pub rs_val: u32,
                    pub rt_val: u32,
                    pub imm: u16,
                }

                #[derive(Clone, Copy, Debug)]
                pub struct JType {
                    pub target: u32,
                }

                #[derive(Clone, Copy, Debug)]
                pub struct RType {
                    pub rs_val: u32,
                    pub rt_val: u32,
                    pub rd: u8,
                    pub shamt: u8,
                    pub funct: u8,
                }
            },
        }
        _def_stage_mod! {
            mod: alu,
            advance_name: access_mem() -> mem,
            instrs: [$($name $alu; $($alu_field_name $alu_field_ty)*);*],
        }
        _def_stage_mod! {
            mod: mem,
            advance_name: write_back(),
            instrs: [$($name $mem; $($mem_field_name $mem_field_ty)*);*],
        }
    };
}

def_stages! {
    Add {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Addi {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Addiu {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Addu {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    And {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Andi {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    BCond {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Beq {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Bgtz {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Blez {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Bne {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Break {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Cop0 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Cop1 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Cop2 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Cop3 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Div {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Divu {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    J {
        rd: { inner: JType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Jal {
        rd: { inner: JType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Jalr {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Jr {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lb {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lbu {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lh {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lhu {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lui {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lw {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lwc0 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lwc1 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lwc2 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lwc3 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lwl {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Lwr {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Mfhi {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Mflo {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Mthi {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Mtlo {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Mult {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Multu {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Nor {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Or {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Ori {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sb {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sh {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sll {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sllv {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Slt {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Slti {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sltiu {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sltu {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sra {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Srav {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Srl {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Srlv {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Srv {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sub {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Subu {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Sw {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Swc0 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Swc1 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Swc2 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Swc3 {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Swl {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Swr {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Syscall {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Xor {
        rd: { inner: RType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
    Xori {
        rd: { inner: IType } => |instr| {
            todo!()
        },
        alu: {} => |instr| {
            todo!()
        },
        mem: {} => |instr| {
            todo!()
        },
    },
}
