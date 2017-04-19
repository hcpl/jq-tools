pub fn is_alphanumeric_(c: char)   -> bool { c.is_alphanumeric() || c == '_' }
pub fn is_digit_dot(c: char)       -> bool { c.is_digit(10) || c == '.' }
pub fn is_alphaunderscore(c: char) -> bool { c.is_alphabetic() || c == '_' }
pub fn is_in(c: char, s: &str)     -> bool { s.contains(c) }


macro_rules! ret_const(
    ($i:expr, $value:expr) => (
        $crate::nom::IResult::Done($i, $value) as $crate::nom::IResult<_, _, u32>
    );
);

macro_rules! peek_cond (
    ($input:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => (
        {
            use $crate::nom::{IResult, ErrorKind};

            let output = $submac2!($input, $($args2)*);
            match output {
                IResult::Done(i, o) => {
                    if $submac!(o, $($args)*) {
                        IResult::Done(i, o)
                    } else {
                        IResult::Error(error_position!(ErrorKind::Custom(150), $input))
                    }
                },
                other => other,
            }
        }
    );
    ($input:expr, $submac:ident!( $($args:tt)* ), $g:expr) => (
        peek_cond!($input, $submac!($($args)*), call!($g));
    );
    ($input:expr, $f:expr, $submac2:ident!( $($args2:tt)* )) => (
        peek_cond!($input, call!($f), $submac2!($($args2)*));
    );
    ($input:expr, $f:expr, $g:expr) => (
        peek_cond!($input, call!($f), call!($g));
    );
);

macro_rules! char_pred (
    ($input:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use $crate::nom::{IResult, ErrorKind, Needed, Slice, AsChar, InputIter};

            match ($input).iter_elements().next().map(|c| $submac!(c, $($args)*)) {
                None        => IResult::Incomplete::<_, _>(Needed::Size(1)),
                Some(false) => IResult::Error(error_position!(ErrorKind::Char, $input)),
                Some(true)  => IResult::Done(
                    $input.slice(1..),
                    $input.iter_elements().next().unwrap().as_char()),
            }
        }
    );
    ($input:expr, $f:expr) => (
        char_pred!($input, call!($f));
    );
);

macro_rules! take_while_m_n (
    ($input:expr, $m:expr, $n:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use std::cmp;
            use $crate::nom::{IResult, ErrorKind, InputLength, InputIter, Slice};
            let input = $input;

            if input.input_len() < $m {
                IResult::Incomplete(Needed::Size($m))
            } else {
                match input.position(|c| !$submac!(c, $($args)*)) {
                    Some(k) => {
                        if $m <= k {
                            let x = cmp::min($n, k);
                            IResult::Done(input.slice(x..), input.slice(..x))
                        } else {
                            IResult::Error(error_position!(ErrorKind::Custom(300), input))
                        }
                    },
                    None => {
                        let x = cmp::min($n, input.input_len());
                        IResult::Done(input.slice(x..), input.slice(..x))
                    },
                }
            }
        }
    );
    ($input:expr, $m:expr, $n:expr, $f:expr) => (
        take_while_m_n!($input, $m, $n, call!($f));
    );
);

macro_rules! take_while_n (
    ($input:expr, $n:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use std::cmp;
            use $crate::nom::{IResult, InputLength, InputIter, Slice};
            let input = $input;

            match input.position(|c| !$submac!(c, $($args)*)) {
                Some(k) => {
                    let x = cmp::min($n, k);
                    IResult::Done(input.slice(x..), input.slice(..x))
                },
                None => {
                    let x = cmp::min($n, input.input_len());
                    IResult::Done(input.slice(x..), input.slice(..x))
                },
            }
        }
    );
    ($input:expr, $n:expr, $f:expr) => (
        take_while_n!($input, $n, call!($f))
    );
);
