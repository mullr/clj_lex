use clj_lex::parse_clj;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let core_clj = std::fs::read_to_string("core.clj").unwrap();
    c.bench_function("parse clore.clj", |b| {
        b.iter(|| parse_clj(black_box(&core_clj)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
