use crate::atoms::{missing, finished, seeded, edges, tc128, tc256, tc512};
use rustler::{Env, Encoder, OwnedBinary, Binary, Term, Error, NifResult, Atom};
use rustler::resource::ResourceArc;
use rustler::types::tuple::get_tuple;
use fountaincode::decoder::{Decoder as FountainDecoder, CatchResult};
use fountaincode::droplet::{Droplet, DropType};
use fountaincode::ldpc::{droplet_decode, DecoderType};
use labrador_ldpc::LDPCCode;
use std::sync::RwLock;
use std::io::Write;

pub struct DecoderRes {
    pub decoder: RwLock<FountainDecoder>
}

pub fn load(env: Env) -> bool {
    rustler::resource!(DecoderRes, env);
    true
}

#[rustler::nif(name = "new_decoder")]
fn new_decoder(len: usize, chunk: usize) -> ResourceArc<DecoderRes> {
    let resource = ResourceArc::new(DecoderRes {
        decoder: RwLock::new(FountainDecoder::new(len, chunk))
    });

    resource
}

#[rustler::nif(name = "catch_drop_ldpc")]
fn catch_drop_ldpc<'a>(
    env: Env<'a>,
    decoder_res: ResourceArc<DecoderRes>,
    drop: Term<'a>,
    decoder_type: Atom) -> NifResult<Term<'a>> {
    let mut dec = decoder_res.decoder.write().unwrap();

    let drop_tuple = get_tuple(drop);

    match drop_tuple {
        Ok(tup) => {
            let first = get_tuple(tup[0]);

            match first {
                Ok(edge_or_seeded) => {
                    if edge_or_seeded[0] == edges().to_term(env) {
                        // Do edges stuff
                        let cnt: usize = edge_or_seeded[1].decode().unwrap();
                        let drop_data = tup[1].into_binary().unwrap();
                        let mut droplet = Droplet::new(DropType::Edges(cnt), drop_data.to_vec());
                        if decoder_type == tc128() {
                            droplet_decode(&mut droplet, LDPCCode::TC128, DecoderType::Ms);
                        } else if decoder_type == tc256() {
                            droplet_decode(&mut droplet, LDPCCode::TC256, DecoderType::Ms);
                        } else if decoder_type == tc512() {
                            droplet_decode(&mut droplet, LDPCCode::TC512, DecoderType::Ms);
                        } else {
                            return Err(Error::BadArg)
                        }
                        match dec.catch(droplet) {
                            CatchResult::Missing(stats) => {
                                Ok(
                                    (missing(),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                            CatchResult::Finished(data, stats) => {
                                let mut binary = OwnedBinary::new(data.len()).unwrap();
                                binary.as_mut_slice().write_all(&data).unwrap();
                                Ok(
                                    (finished(),
                                    Binary::from_owned(binary, env),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                        }
                    } else if edge_or_seeded[0] == seeded().to_term(env) {
                        // Do seeded stuff
                        let seed: u64 = edge_or_seeded[1].decode().unwrap();
                        let degree: usize = edge_or_seeded[2].decode().unwrap();
                        let drop_data = tup[2].into_binary().unwrap();
                        let mut droplet = Droplet::new(DropType::Seeded(seed, degree), drop_data.to_vec());
                        if decoder_type == tc128() {
                            droplet_decode(&mut droplet, LDPCCode::TC128, DecoderType::Ms);
                        } else if decoder_type == tc256() {
                            droplet_decode(&mut droplet, LDPCCode::TC256, DecoderType::Ms);
                        } else if decoder_type == tc512() {
                            droplet_decode(&mut droplet, LDPCCode::TC512, DecoderType::Ms);
                        } else {
                            return Err(Error::BadArg)
                        }

                        match dec.catch(droplet) {
                            CatchResult::Missing(stats) => {
                                Ok(
                                    (missing(),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                            CatchResult::Finished(data, stats) => {
                                let mut binary = OwnedBinary::new(data.len()).unwrap();
                                binary.as_mut_slice().write_all(&data).unwrap();
                                Ok(
                                    (finished(),
                                    Binary::from_owned(binary, env),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                        }
                    } else {
                        Err(Error::BadArg)
                    }
                }
                _ =>
                    Err(Error::BadArg)
            }
        }
        _ =>
            Err(Error::BadArg)
    }
}

#[rustler::nif(name = "catch_drop")]
fn catch_drop<'a>(
    env: Env<'a>,
    decoder_res: ResourceArc<DecoderRes>,
    drop: Term<'a>) -> NifResult<Term<'a>> {
    let mut dec = decoder_res.decoder.write().unwrap();

    let drop_tuple = get_tuple(drop);

    match drop_tuple {
        Ok(tup) => {
            let first = get_tuple(tup[0]);

            match first {
                Ok(edge_or_seeded) => {
                    if edge_or_seeded[0] == edges().to_term(env) {
                        // Do edges stuff
                        let cnt: usize = edge_or_seeded[1].decode().unwrap();
                        let drop_data = tup[1].into_binary().unwrap();
                        let droplet = Droplet::new(DropType::Edges(cnt), drop_data.to_vec());
                        match dec.catch(droplet) {
                            CatchResult::Missing(stats) => {
                                Ok(
                                    (missing(),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                            CatchResult::Finished(data, stats) => {
                                let mut binary = OwnedBinary::new(data.len()).unwrap();
                                binary.as_mut_slice().write_all(&data).unwrap();
                                Ok(
                                    (finished(),
                                    Binary::from_owned(binary, env),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                        }
                    } else if edge_or_seeded[0] == seeded().to_term(env) {
                        // Do seeded stuff
                        let seed: u64 = edge_or_seeded[1].decode().unwrap();
                        let degree: usize = edge_or_seeded[2].decode().unwrap();
                        let drop_data = tup[2].into_binary().unwrap();
                        let droplet = Droplet::new(DropType::Seeded(seed, degree), drop_data.to_vec());

                        match dec.catch(droplet) {
                            CatchResult::Missing(stats) => {
                                Ok(
                                    (missing(),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                            CatchResult::Finished(data, stats) => {
                                let mut binary = OwnedBinary::new(data.len()).unwrap();
                                binary.as_mut_slice().write_all(&data).unwrap();
                                Ok(
                                    (finished(),
                                    Binary::from_owned(binary, env),
                                    (stats.cnt_droplets, stats.cnt_chunks, stats.overhead, stats.unknown_chunks)).encode(env)
                                )
                            }
                        }
                    } else {
                        Err(Error::BadArg)
                    }
                }
                _ =>
                    Err(Error::BadArg)
            }
        }
        _ =>
            Err(Error::BadArg)
    }
}
