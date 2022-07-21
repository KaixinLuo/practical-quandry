extern crate nom;

use crate::node::Node;
use crate::expr::expr;
use nom::branch::alt;
use nom::character::streaming::*;
use nom::bytes::streaming::tag;
use nom::combinator::{map,value,recognize,opt};
use nom::multi::{many0,many1,separated_list0,separated_list1};
use nom::sequence::{delimited, pair,preceded,terminated};
use nom::{IResult};

fn ret_stmt(input:&str)->IResult<&str,Box<Node>>{
    let token = |word|{preceded(multispace0, word)}
    if let Ok((input1,indents)) = space0(input){
        if let Ok((input2,expr)) = expr(input1){
            if let Ok((input3, "\n")) = tag("\n")(input2){
                return Ok((input3,Box::new(Node::ExprStmt(indents.len(), expr))));
            }
        }
    }

    return Error("expr stmt parsing error");
}

fn expr_stmt(input:&str)->IResult<&str,Box<Node>>{
    if let Ok((input1,indents)) = space0(input){
        if let Ok((input2,expr)) = expr(input1){
            if let Ok((input3, "\n")) = tag("\n")(input2){
                return Ok((input3,Box::new(Node::ExprStmt(indents.len(), expr))));
            }
        }
    }

    return Error("expr stmt parsing error");
}