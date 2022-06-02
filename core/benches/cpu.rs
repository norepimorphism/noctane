// SPDX-License-Identifier: MPL-2.0

use criterion::{criterion_group, criterion_main, Criterion};

fn execute_next_instr(c: &mut Criterion) {
    let mut core = noctane::Core::default();
    let mut cpu = core.cpu();

    c.bench_function("cpu::execute_next_instr", |b| {
        b.iter(|| cpu.execute_next_instr())
    });
}

criterion_group!(benches, execute_next_instr);
criterion_main!(benches);
