use criterion::{criterion_group, criterion_main, Criterion};

fn execute_next_instr(c: &mut Criterion) {
    let mut core = noctane::Core::new();
    let cpu = core.cpu_mut();

    c.bench_function(
        "cpu::execute_next_instr",
        |b| b.iter(|| cpu.execute_next_instr()),
    );
}

criterion_group!(benches, execute_next_instr);
criterion_main!(benches);
