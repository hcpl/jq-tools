use nom::IResult;

use ast::*;
use nom_helpers::*;


named!(pub format<&str, Format>,
    do_parse!(
        char!('@') >>
        format: take_while1!(is_alphanumeric_) >>
        //(Format { format: format })
        (format.to_string())
    )
);

pub fn literal(input: &str) -> IResult<&str, String> {
    do_parse!(input,
        a: take_while1!(is_digit_dot) >>
        b: opt!(do_parse!(
            one_of!("eE") >>
            y: opt!(one_of!("+-")) >>
            z: take_while1!(apply!(char::is_digit, 10)) >>
            (1 + y.is_some() as usize + z.len())
        )) >>
        ({
            let outlen = a.len() + b.unwrap_or(0);
            input[0..outlen].to_string()
        })
    )
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    do_parse!(input,
        prefix: many0!(complete!(do_parse!(
            char_pred!(is_alphaunderscore) >>
            next_chars: take_while!(is_alphanumeric_) >>
            tag!("::") >>
            (1 + next_chars.len() + 2)
        ))) >>
        char_pred!(is_alphaunderscore) >>
        next_chars: take_while!(is_alphanumeric_) >>
        ({
            let outlen = prefix.into_iter().sum::<usize>() + 1 + next_chars.len();
            &input[0..outlen]
        })
    )
}

pub fn field(input: &str) -> IResult<&str, &str> {
    do_parse!(input,
        char!('.') >>
        char_pred!(is_alphaunderscore) >>
        next_chars: take_while!(is_alphanumeric_) >>
        ({
            let outlen = 1 + 1 + next_chars.len();
            &input[0..outlen]
        })
    )
}
