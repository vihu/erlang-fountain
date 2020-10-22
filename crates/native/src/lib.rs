extern crate fountaincode;
extern crate rustler;

use rustler::{Env, Term};

mod atoms;
mod encoder;
mod decoder;

fn load(env: Env, _: Term) -> bool {
    encoder::load(env);
    decoder::load(env);
    true
}

rustler::init!(
    "erlang_fountain",
    [
        encoder::new_encoder,
        encoder::new_ldpc_encoder,
        encoder::next,
        decoder::new_decoder,
        decoder::catch_drop,
        decoder::catch_drop_ldpc,
    ],
    load=load
);
