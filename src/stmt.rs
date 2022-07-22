extern crate nom;

use crate::node::Node;
use crate::expr::expr;
use nom::branch::alt;
use nom::character::streaming::*;
use nom::bytes::streaming::tag;
use nom::combinator::{map,value,recognize,opt,peek};
use nom::error::Error;
use nom::multi::{many0,many1,separated_list0,separated_list1};
use nom::sequence::{delimited, pair,preceded,terminated,tuple};
use nom::{IResult};

pub fn stmt(input:&str)->IResult<&str,Box<Node>>{
    alt((ret_stmt,expr_stmt,if_stmt,loop_stmt))(input)
}
fn loop_stmt(input:&str)->IResult<&str,Box<Node>>{
    
    let token = |word|{preceded(multispace0, word)};
    let mut while_parser = map(tuple((space0,token(tag("for")),expr,pair(delimited(space0,tag(":"),space0),tag("\n")),block)),|(indent,_,condition,_,body)|{Box::new(Node::LoopStmt(indent.len(), condition, body))});
    while_parser(input)
}
fn if_stmt(input:&str)->IResult<&str,Box<Node>>{
    
    let token = |word|{preceded(multispace0, word)};
    let if_header = map(tuple((space0,token(tag("if")),expr,pair(delimited(space0,tag(":"),space0),tag("\n")),block)),|(indent,_,condition,_,if_true)|{(indent.len(),condition,if_true)});
    let else_with_condition_parser = map(tuple((space0,token(tag("else")),expr,pair(delimited(space0,tag(":"),space0),tag("\n")),block)),|(indent,_,condition,_,if_true)|{(condition,if_true)});
    let else_parser = map(tuple((space0,token(tag("else")),pair(delimited(space0,tag(":"),space0),tag("\n")),block)),|(indent,_,_,if_true)|{(if_true)});
    let mut if_complete_parser = map(tuple((if_header,many0(else_with_condition_parser),else_parser)),|((level,if_condition,if_block),mut list_of_else, last_else)|{
        let mut result = Vec::new();
        result.push((if_condition,if_block));
        result.append(&mut list_of_else);
        result.push((Box::new(Node::Empty()),last_else));
        Box::new(Node::IfStmt(level, result))
    }); 
    if_complete_parser(input)
}
fn block(input:&str)->IResult<&str,Box<Node>>{
    let mut probe = |input|{
        let (_,indent_level)=map(peek(space0::<&str,Error<&str>>),|indents|{
            indents.len()})(input).unwrap();
        indent_level};
    let initial_indent_level = probe(input);
    let mut current_indient_level = initial_indent_level;
    let mut stmts:Vec<Box<Node>> = Vec::new();
    let mut context = input;
    while current_indient_level==initial_indent_level{
        let (next_context,stmt_node) = stmt(context).unwrap();
        stmts.push(stmt_node);
        context = next_context;
        current_indient_level = probe(context);
    }
    Ok((context,Box::new(Node::Block(stmts))))
}

fn ret_stmt(input:&str)->IResult<&str,Box<Node>>{
    let token = |word|{preceded(multispace0, word)};
    map(tuple((space0,token(tag("return")),expr,many1(pair(space0,tag("\n"))))),
        |(indents,_,expr_node,_)|{
            Box::new(Node::ReturnStmt(indents.len(),expr_node))})(input)
}

fn expr_stmt(input:&str)->IResult<&str,Box<Node>>{
    map(tuple((space0,expr,many1(pair(space0,tag("\n"))))),
        |(indents,expr_node,_)|{
            Box::new(Node::ExprStmt(indents.len(),expr_node))})(input)
}