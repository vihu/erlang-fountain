extern crate fountaincode;

extern crate rustler;

use rustler::{Encoder, Env, Error, Term};
use rustler::resource::ResourceArc;
use fountaincode::ltcode::Encoder as FountainEncoder;
use fountaincode::ltcode::Decoder as FountainDecoder;
use fountaincode::ltcode::EncoderType;
use fountaincode::ltcode::CatchResult::{Missing, Finished};

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
    }
}

rustler::rustler_export_nifs!(
    "erlang_fountain",
    [
        ("encode_native_systematic", 2, encode_native_systematic),
        ("encode_native_random", 2, encode_native_random),
        ("decode_native", 3, decode_native)
    ],
    Some(on_load)
);

struct EncodeResource {
    pub encoder: FountainEncoder
}

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource_struct_init!(EncodeResource, env);
    true
}

fn encode_native_systematic<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let buf: Vec<u8> = args[0].decode()?;
    let chunk: usize = args[1].decode()?;
    let resource = ResourceArc::new(EncodeResource {
        encoder: FountainEncoder::new(buf, chunk, EncoderType::Systematic)
    });

    Ok((atoms::ok(), resource).encode(env))
}

fn encode_native_random<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let buf: Vec<u8> = args[0].decode()?;
    let length: usize = args[1].decode()?;
    let resource = ResourceArc::new(EncodeResource {
        encoder: FountainEncoder::new(buf, length, EncoderType::Random)
    });

    Ok((atoms::ok(), resource).encode(env))
}

fn decode_native<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let resource: ResourceArc<EncodeResource> = args[0].decode()?;
    let enc = resource.encoder.clone();
    let len: usize = args[1].decode()?;
    let chunk: usize = args[2].decode()?;
    let mut dec = FountainDecoder::new(len, chunk);

    let mut data: Vec<u8> = Vec::new();
    for drop in enc {
        match dec.catch(drop) {
            Missing(_stats) => {
                continue
            }
            Finished(data0, _stats) => {
                data = data0;
                break
            }
        }
    }

    Ok((atoms::ok(), data).encode(env))
}
