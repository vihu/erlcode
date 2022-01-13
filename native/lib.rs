use rustler::{Encoder, Env, NifResult, Term};

rustler::atoms! {
    ok,
    error,
}

#[rustler::nif(name = "golay_extended_encode")]
pub fn golay_extended_encode<'a>(env: Env<'a>, data: u16) -> NifResult<Term<'a>> {
    let e = cai_golay::extended::encode(data);
    Ok((ok(), e).encode(env))
}

#[rustler::nif(name = "golay_standard_encode")]
pub fn golay_standard_encode<'a>(env: Env<'a>, data: u16) -> NifResult<Term<'a>> {
    let e = cai_golay::standard::encode(data);
    Ok((ok(), e).encode(env))
}

#[rustler::nif(name = "golay_shortened_encode")]
pub fn golay_shortened_encode<'a>(env: Env<'a>, data: u8) -> NifResult<Term<'a>> {
    let e = code_rs::coding::golay::shortened::encode(data);
    Ok((ok(), e).encode(env))
}

fn load(_env: Env, _: Term) -> bool {
    true
}

rustler::init!(
    "erlcode",
    [
        golay_extended_encode,
        golay_standard_encode,
        golay_shortened_encode
    ],
    load = load
);
