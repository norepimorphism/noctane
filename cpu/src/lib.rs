// SPDX-License-Identifier: MPL-2.0

//! The PSX Central Processing Unit (CPU).
//!
//! This module implements CPU-related types and functions, which are considered to encompass the
//! following:
//! - CPU instruction fetching, decoding, and execution
//! - CPU bus and associated memory interface
//! - CPU and COP0 registers
//! - CPU I-cache
//!
//! # Bus
//!
//! As the inclusion of the CPU bus implies, this module defines an interface from the CPU to
//! external devices such as the GPU, SPU, and dual serial ports (DUART). *However*, this module
//! does not *implement* those devices; it merely references them. To see the specifics of this
//! aforementioned interface, see [`bus`] and [`bus::io`].
//!
//! # Memory
//!
//! CPU memory was implemented in a tiered design. When a [`Cpu`] performs a memory access, its
//! [`Memory`] is the first to be notified. From there, if the program address points to a
//! cacheable address region, the [`ICache`] is consulted to determine if a valid cached copy of the
//! requested data exists. If so, it is retrieved from there, and the access is complete. Otherwise,
//! the access continues to the [`Bus`], at which point the specific memory bank being addressed is
//! selected and either read from or written to.

// Hereafter, this module will use the acronym RM to refer specifically to the *IDT R30xx Family
// Software Reference Manual*, the version of which obtainable from [here] was heavily drawn upon to
// develop Noctane's CPU implementation. Although the PSX technically contains a customized CoreWare
// CW33000, it is largely compatible with the R3000, and I could not find a more comprehensive
// resource than what IDT has provided.
//
// Furthermore, the convention RM[c-p] will reference a specific page (p) within a chapter (c) of
// the RM. For example, RM[13-1] references the first page of the 13th chapter, which is titled
// "Instruction Timing and Optimization".
//
// The terms 'address region' and 'program address' are the same as those specified in the RM. See
// the "Glossary of Common 'MIPS' Terms" at Appendix F for the precise definitions. In essence,
// however, they are roughly defined as follows:
// - **program address**: synonymous with 'virtual address'. Program addresses are 'above' physical
//   addresses in terms of abstraction. A program address is translated into a physical address in
//   the [`mem`] module, where it is determined to select one of the four address regions and
//   rebased to the start of that region, thus forming a physical address.
// - **address region**: MIPS I divides the address space, or the range of available program
//   addresses, into four 'address regions' named *kuseg*, *kseg0*, *kseg1*, and *kseg2*. As
//   discussed in the definition of 'program address', these regions are only relevant when
//   referring to program addresses. *kuseg* and *kseg0* are 'cacheable', which means the I-cache
//   is first consulted for all accesses (unless it is disabled, of course).
//
// [here]: https://cgi.cse.unsw.edu.au/~cs3231/doc/R3000.pdf

#![feature(
    // [`u32::widening_mul`] is used once in [`instr`].
    bigint_helper_methods,
    // `box` syntax is used to avoid overflowing the stack while allocating very large chunks of
    // heap memory for the memory banks.
    box_syntax,
    // `let x = y else { z }` proves extremely helpful in error handling.
    let_else,
    // The [`slice::as_chunks`] method is used frequently to decompose a 32-bit word into two 16-bit
    // halfwords.
    slice_as_chunks,
    // `unchecked_mul` is used once in [`instr`].
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

/// The state of a [`Cpu`].
///
/// This type does little on its own. However, it is the ancestor of all `Cpu`s. To produce a
/// `Cpu` from a `State`, use the [`connect_bus`] method.
///
/// [`connect_bus`]: Self::connect_bus
#[derive(Default)]
pub struct State {
    /// The I-cache and scratchpad, collectively.
    ///
    /// When the CPU accesses a cacheable address region, it first consults the instruction cache
    /// (I-cache). If a valid cached copy of the requested data is available, it is fetched, and the
    /// access is 'invisible' to the outside world (i.e., the CPU bus).
    ///
    /// The scratchpad, on the other hand, is like any other memory bank on the CPU bus, except it
    /// is contained within the CPU. Supposedly, Sony mapped the data cache (D-cache) to the
    ///
    /// See the module documntation for more information.
    pub cache: Cache,
    /// The instruction pipeline.
    ///
    /// Per the MIPS I architecture, instructions are 'pipelined' in which up to five instructions
    /// may be simultaneously queued in varying stages of execution and each advanced to the next
    /// stage at every clock cycle. This field contains that queue.
    pub pipeline: instr::Pipeline,
    /// The register file.
    ///
    /// This contains all of the CPU's, as well as COP0's, registers. That includes the program
    /// counter (PC), HI and LO, general purpose registers, and control registers.
    pub reg: reg::File,
}

impl State {
    /// Produces a [`Cpu`] from a `State`.
    ///
    /// On its own, `State` is just data. But, by attaching the CPU bus, it becomes 'connected', and
    /// hence capable of properly fetching, decoding, and executing instructions. Because the data
    /// contained within `Cpu`s is backed by an inner `State`, the lifetime of `Cpu` is bound to the
    /// `State` that produced it.
    ///
    /// This method is very cheap, so don't shy away from calling it before executing every next
    /// instruction if necessary.
    pub fn connect_bus<'s, 'b>(&'s mut self, bus: crate::Bus<'b>) -> Cpu<'s, 'b> {
        Cpu {
            pipeline: &mut self.pipeline,
            reg: &mut self.reg,
            mem: Memory::new(&mut self.cache, bus),
        }
    }
}

/// The PSX Central Processing Unit (CPU).
///
/// This type is produced by calling the [`State::connect_bus`] method. A `Cpu`'s lifetime is thus
/// bound to that of the `State` that produced it.
///
/// To get started with executing instructions, see the [`execute_next_instr`],
/// [`execute_instr`], and [`execute_opcode`] methods. If custom instruction decoding is
/// desired, see [`advance_pipeline`].
///
/// See the module documentation for more information.
///
/// [`execute_next_instr`]: Self::execute_next_instr
/// [`execute_instr`]: Self::execute_instr
/// [`execute_opcode`]: Self::execute_opcode
/// [`advance_pipeline`]: Self::advance_pipeline
pub struct Cpu<'s, 'b> {
    pipeline: &'s mut instr::Pipeline,
    reg: &'s mut reg::File,
    mem: Memory<'s, 'b>,
}

impl<'s, 'b> Cpu<'s, 'b> {
    /// A shared reference to this CPU's instruction pipeline.
    pub fn pipeline(&self) -> &instr::Pipeline {
        &self.pipeline
    }

    /// An exclusive reference to this CPU's instruction pipeline.
    pub fn pipeline_mut(&mut self) -> &mut instr::Pipeline {
        &mut self.pipeline
    }

    /// A shared reference to this CPU's register file.
    pub fn reg(&self) -> &reg::File {
        &self.reg
    }

    /// An exclusive reference to this CPU's register file.
    pub fn reg_mut(&mut self) -> &mut reg::File {
        &mut self.reg
    }

    /// A shared reference to this CPU's memory.
    pub fn mem(&self) -> &Memory<'s, 'b> {
        &self.mem
    }

    /// An exclusive reference to this CPU's memory.
    pub fn mem_mut(&mut self) -> &mut Memory<'s, 'b> {
        &mut self.mem
    }

    /// Executes the instruction referenced by the program counter (PC).
    ///
    /// This method returns the result of the execution.
    ///
    /// To view the current value of the PC, see the [`reg`] method. Or, to execute a specific
    /// instruction or opcode, see [`execute_instr`] and [`execute_opcode`], respectively.
    ///
    /// [`reg`]: Self::reg
    /// [`execute_instr`]: Self::execute_instr
    /// [`execute_opcode`]: Self::execute_opcode
    pub fn execute_next_instr(&mut self) -> instr::Execution {
        // TODO: Don't unwrap!
        self.advance_pipeline(|op| Instr::decode(op).unwrap())
    }

    /// Executes the given instruction.
    ///
    /// This method returns the result of the execution.
    pub fn execute_instr(&mut self, instr: Instr) -> instr::Execution {
        self.advance_pipeline(|_| instr)
    }

    /// Executes the instruction decodeable from the given opcode.
    ///
    /// This method returns the result of the execution.
    pub fn execute_opcode(&mut self, op: u32) -> instr::Execution {
        // TODO: Don't unwrap!
        self.advance_pipeline(|_| Instr::decode(op).unwrap())
    }

    /// Processes the current stage, and then advances to the next stage, of each instruction
    /// queued in the instruction pipeline.
    ///
    /// The `decode_instr` argument decodes a given opcode into an instruction. A sane default for
    /// this function, which is used in [`execute_next_instr`] and [`execute_opcode`], is
    /// to call upon [`Instr::decode`].
    ///
    /// [`execute_next_instr`]: Self::execute_next_instr
    /// [`execute_opcode`]: Self::execute_opcode
    pub fn advance_pipeline(
        &mut self,
        decode_instr: impl Fn(u32) -> Instr,
    ) -> instr::Execution {
        // Actually advance the pipeline.
        let exec =
            self.pipeline
                .advance(&mut self.mem, &mut self.reg, &decode_instr);

        // If the status register (SR) was modified, we need to apply the changes before returning
        // control back to the caller.
        if let Some(sr) = self.reg.altered_sr() {
            let sr = reg::cpr::Status(sr);

            self.mem.cache_mut().i.set_isolated(sr.is_c());

            if sr.sw_c() {
                // This doesn't do anything in the PSX as far as I know. The I-cache is more-or-less
                // configured to function as a D-cache, so it is basically already 'swapped'.
            }

            // TODO
        }

        exec
    }
}
