use rustler::{Encoder, Env, NifResult, Term};

rustler::atoms! {
    ok,
    error,
    unrecoverable,
}

#[rustler::nif(name = "golay_extended_encode")]
pub fn golay_extended_encode<'a>(env: Env<'a>, data: u16) -> NifResult<Term<'a>> {
    let e = cai_golay::extended::encode(data);
    Ok((ok(), e).encode(env))
}

#[rustler::nif(name = "golay_extended_decode")]
pub fn golay_extended_decode<'a>(env: Env<'a>, data: u32) -> NifResult<Term<'a>> {
    match cai_golay::extended::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err)).encode(env)),
        None => Ok((error(), unrecoverable()).encode(env)),
    }
}

#[rustler::nif(name = "golay_standard_encode")]
pub fn golay_standard_encode<'a>(env: Env<'a>, data: u16) -> NifResult<Term<'a>> {
    let e = cai_golay::standard::encode(data);
    Ok((ok(), e).encode(env))
}

#[rustler::nif(name = "golay_standard_decode")]
pub fn golay_standard_decode<'a>(env: Env<'a>, data: u32) -> NifResult<Term<'a>> {
    match cai_golay::standard::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err)).encode(env)),
        None => Ok((error(), unrecoverable()).encode(env)),
    }
}

#[rustler::nif(name = "golay_shortened_encode")]
pub fn golay_shortened_encode<'a>(env: Env<'a>, data: u8) -> NifResult<Term<'a>> {
    let e = code_rs::coding::golay::shortened::encode(data);
    Ok((ok(), e).encode(env))
}

#[rustler::nif(name = "golay_shortened_decode")]
pub fn golay_shortened_decode<'a>(env: Env<'a>, data: u32) -> NifResult<Term<'a>> {
    match code_rs::coding::golay::shortened::decode(data) {
        Some((data, err)) => Ok((ok(), (data, err)).encode(env)),
        None => Ok((error(), unrecoverable()).encode(env)),
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
        golay_shortened_decode
    ],
    load = load
);
