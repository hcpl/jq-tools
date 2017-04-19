#[macro_use]
extern crate nom;

mod ast;
#[macro_use] mod nom_helpers;
#[macro_use] mod lexer;
mod parser;

fn main() {
    println!("Before parsing...");
    println!("{:?}", parser::top_level("."));
    println!("After parsing!");
}
