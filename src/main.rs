
extern crate nom;
mod expr;
mod node;
use nom::branch::alt;
use nom::character::complete::*;
use nom::bytes::complete::tag;
use nom::combinator::{map,value,recognize,opt};
use nom::multi::{many0,many1,separated_list0,separated_list1};
use nom::sequence::{delimited, pair,preceded,terminated,tuple};
use nom::IResult;
use node::Node;
use expr::parse;


fn main() {
   let res = parse("let main = \\args -> println(hello_world)
                                  ");
   if let Ok((_,out)) = res{
      match *out{
         Node::Boolean(literal)=>{println!("Boolean: {}",literal);}
         Node::Integer(literal)=>{println!("Integer: {}",literal);}
         Node::Ident(literal)=>{println!("Ident: {}",literal);}
         _=>{println!("{:?}",out);}
      }
   }else{
      println!("Error{:?}",res);
   }
}
