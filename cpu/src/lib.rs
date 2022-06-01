// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(
    bigint_helper_methods,
    box_syntax,
    core_intrinsics,
    exclusive_range_pattern,
    let_else,
    slice_as_chunks,
    type_alias_impl_trait,
    unchecked_math,
)]

pub mod bus;
pub mod cache;
pub mod exc;
pub mod instr;
pub mod mem;
pub mod reg;

pub use bus::Bus;
pub use cache::{i::Cache as ICache, Cache};
pub use instr::Instr;
pub use mem::Memory;

impl Default for State {
    fn default() -> Self {
        Self {
            cache: Cache::default(),
            exc: exc::Queue::new(),
            pipeline: instr::Pipeline::default(),
            reg: reg::File::default(),
        }
    }
}

pub struct State {
    pub cache: Cache,
    /// The exception queue.
    pub exc: exc::Queue,
    /// The instruction pipeline.
    pub pipeline: instr::Pipeline,
    /// The register file.
    pub reg: reg::File,
}

impl State {
    pub fn connect_bus<'s, 'b>(&'s mut self, bus: crate::Bus<'b>) -> Cpu<'s, 'b> {
        Cpu {
            exc: &mut self.exc,
            pipeline: &mut self.pipeline,
            reg: &mut self.reg,
            mem: Memory::new(&mut self.cache, bus),
        }
    }
}

pub struct Cpu<'s, 'b> {
    exc: &'s mut exc::Queue,
    pipeline: &'s mut instr::Pipeline,
    reg: &'s mut reg::File,
    mem: Memory<'s, 'b>,
}

impl<'s, 'b> Cpu<'s, 'b> {
    pub fn pipeline(&self) -> &instr::Pipeline {
        &self.pipeline
    }

    pub fn pipeline_mut(&mut self) -> &mut instr::Pipeline {
        &mut self.pipeline
    }

    pub fn reg(&self) -> &reg::File {
        &self.reg
    }

    pub fn reg_mut(&mut self) -> &mut reg::File {
        &mut self.reg
    }

    pub fn mem(&self) -> &Memory<'s, 'b> {
        &self.mem
    }

    pub fn mem_mut(&mut self) -> &mut Memory<'s, 'b> {
        &mut self.mem
    }

    pub fn execute_next_instr(&mut self) -> Option<instr::Execution> {
        self.advance_pipeline(|op| Instr::decode(op).unwrap())
    }

    pub fn execute_instr(&mut self, instr: Instr) -> Option<instr::Execution> {
        self.advance_pipeline(|_| instr)
    }

    pub fn execute_opcode(&mut self, op: u32) -> Option<instr::Execution> {
        self.advance_pipeline(|_| Instr::decode(op).unwrap())
    }

    /// Processes the current stage, and then advances to the next stage, of each queued
    /// instruction.
    pub fn advance_pipeline(
        &mut self,
        decode_instr: impl Fn(u32) -> Instr,
    ) -> Option<instr::Execution> {
        self.handle_exc();

        let instr = self.pipeline.advance(
            &mut self.exc,
            &mut self.mem,
            &mut self.reg,
            &decode_instr,
        );

        self.reg.apply_cpr(|idx, value| {
            match idx {
                reg::cpr::BAD_VADDR_IDX => {
                    todo!()
                }
                reg::cpr::STATUS_IDX => {
                    let sr = reg::cpr::Status(value);

                    self.mem.cache_mut().i.set_isolated(sr.is_c());

                    if sr.sw_c() {
                        // This doesn't do anything on the PSX as far as I know. The I-cache is
                        // basically already configured to function as a D-cache, so it is already
                        // 'swapped'.
                    }

                    // TODO
                }
                reg::cpr::CAUSE_IDX => {
                    // TODO
                }
                reg::cpr::EPC_IDX => {
                    todo!()
                }
                _ => {
                    // We don't care, lol.
                }
            }
        });

        instr
    }

    fn handle_exc(&mut self) {
        if let Ok(e) = self.exc.pop() {
            tracing::error!("Exception: {:?}", e);
            *self.reg.pc_mut() = e.vector();
        }
    }
}
