use rustler::{error::Error, Atom, Env, NifResult, Term};

rustler::atoms! {
    ok,
    error,
    unrecoverable,
}

#[rustler::nif(name = "golay_extended_encode")]
pub fn golay_extended_encode(data: u16) -> NifResult<(Atom, u32)> {
    let e = cai_golay::extended::encode(data);
    Ok((ok(), e))
}

#[rustler::nif(name = "golay_extended_decode")]
pub fn golay_extended_decode(data: u32) -> NifResult<(Atom, (u16, usize))> {
    match cai_golay::extended::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err))),
        None => Err(Error::Term(Box::new(unrecoverable()))),
    }
}

#[rustler::nif(name = "golay_standard_encode")]
pub fn golay_standard_encode(data: u16) -> NifResult<(Atom, u32)> {
    let e = cai_golay::standard::encode(data);
    Ok((ok(), e))
}

#[rustler::nif(name = "golay_standard_decode")]
pub fn golay_standard_decode(data: u32) -> NifResult<(Atom, (u16, usize))> {
    match cai_golay::standard::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err))),
        None => Err(Error::Term(Box::new(unrecoverable()))),
    }
}

#[rustler::nif(name = "golay_shortened_encode")]
pub fn golay_shortened_encode(data: u8) -> NifResult<(Atom, u32)> {
    let e = code_rs::coding::golay::shortened::encode(data);
    Ok((ok(), e))
}

#[rustler::nif(name = "golay_shortened_decode")]
pub fn golay_shortened_decode(data: u32) -> NifResult<(Atom, (u8, usize))> {
    match code_rs::coding::golay::shortened::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err))),
        None => Err(Error::Term(Box::new(unrecoverable()))),
    }
}

#[rustler::nif(name = "bch_encode")]
pub fn bch_encode(data: u16) -> NifResult<(Atom, u64)> {
    let e = code_rs::coding::bch::encode(data);
    Ok((ok(), e))
}

#[rustler::nif(name = "bch_decode")]
pub fn bch_decode(data: u64) -> NifResult<(Atom, (u16, usize))> {
    match code_rs::coding::bch::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err))),
        None => Err(Error::Term(Box::new(unrecoverable()))),
    }
}

fn load(_env: Env, _: Term) -> bool {
    true
}

rustler::init!(
    "erlcode",
    [
        golay_extended_encode,
        golay_extended_decode,
        golay_standard_encode,
        golay_standard_decode,
        golay_shortened_encode,
        golay_shortened_decode,
        bch_encode,
        bch_decode
    ],
    load = load
);
