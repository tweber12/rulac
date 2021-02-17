use criterion::{criterion_group, criterion_main, Criterion};

use helac_rs::color::flow::{ColorFlow, ColorFlows};
use helac_rs::skeleton::Builder;
use helac_rs::ufo::{PdgCode, UfoModel};

fn convert_particles(codes: &[i64]) -> Vec<PdgCode> {
    codes.iter().map(|i| PdgCode(*i)).collect()
}

fn bench_epem_epem_colored(c: &mut Criterion) {
    let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
    let incoming = convert_particles(&[-11, 11]);
    let outgoing = convert_particles(&[-11, 11]);
    let flows = ColorFlows::new(&incoming, &outgoing, &model);
    let flow = flows.iter().next().unwrap();
    let mut builder = Builder::new(&incoming, &outgoing, &model).unwrap();
    c.bench_function("epem_epem_colored", |b| {
        b.iter(|| builder.get_skeleton(flow))
    });
}

fn bench_gg_bbx_colored(c: &mut Criterion) {
    let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
    let incoming = convert_particles(&[21, 21]);
    let outgoing = convert_particles(&[5, -5]);
    let flows = ColorFlows::new(&incoming, &outgoing, &model);
    let flow = flows.iter().next().unwrap();
    let mut builder = Builder::new(&incoming, &outgoing, &model).unwrap();
    c.bench_function("gg_bbx_colored", |b| b.iter(|| builder.get_skeleton(flow)));
}

fn bench_gg_epvemumvmxbbxgg_colored(c: &mut Criterion) {
    let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
    let incoming = convert_particles(&[21, 21]);
    let outgoing = convert_particles(&[-11, 12, 13, -14, 5, -5, 21, 21]);
    let flows = ColorFlows::new(&incoming, &outgoing, &model);
    let flow = flows.iter().next().unwrap();
    let mut builder = Builder::new(&incoming, &outgoing, &model).unwrap();
    c.bench_function("gg_epvemumvmxbbxgg_colored", |b| {
        b.iter(|| builder.get_skeleton(flow))
    });
}

criterion_group!(
    colored,
    bench_epem_epem_colored,
    bench_gg_bbx_colored,
    bench_gg_epvemumvmxbbxgg_colored
);
criterion_main!(colored);
