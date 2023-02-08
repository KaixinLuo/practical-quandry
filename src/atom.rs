use crate::expr::expr;
use crate::node::Node;
use nom::branch::alt;
use nom::character::complete::*;
use nom::bytes::complete::tag;
use nom::combinator::{map,value,recognize};
use nom::multi::many0;
use nom::sequence::{pair,preceded, delimited};
use nom::IResult;

pub fn atom(input:&str)->IResult<&str,Box<Node>>{
     alt((
         literal,
         ident,
         lift
       ))(input)
  }
fn lift(input:&str)->IResult<&str,Box<Node>>{
    let token = |word|{preceded(multispace0, word)};
    map(
        delimited(
            token(char('(')), 
            expr, 
            token(char(')'))),|result|{result})(input)
}
  fn ident(input:&str)->IResult<&str,Box<Node>>{
     map(
        preceded(multispace0, recognize(pair(alt((tag("_"),alpha1)),many0(alt((tag("_"),alphanumeric1)))))), 
        |literal:&str|{
           Box::new(Node::Ident(String::from(literal)))
        }
     )(input)
  }
  
  fn literal(input:&str)->IResult<&str,Box<Node>>{
     alt((
        integer,
        boolean,
     ))(input)
  }

  fn integer(input:&str)->IResult<&str,Box<Node>>{
      map(
          preceded(multispace0, digit1),
          |literal:&str|{
              Box::new(Node::Integer(literal.parse::<i64>().unwrap()))
          })(input)
  }
  
  fn boolean(input:&str)->IResult<&str,Box<Node>>{
    let token = |word|{preceded(multispace0, tag(word))};
      alt((
         value(Box::new(Node::Boolean(true)),token("true")),
         value(Box::new(Node::Boolean(false)),token("false"))
      ))(input)
  }
 
  #[cfg(test)]
 mod tests {
     // Note this useful idiom: importing names from outer (for mod tests) scope.
     use super::*;
     #[test]
     fn test_bool_true_tight() {
         assert_eq!(atom("true"), Ok(("",Box::new(Node::Boolean(true)))));
     }
 
     #[test]
     fn test_bool_false_tight() {
         // This assert would fire and test will fail.
         // Please note, that private functions can be tested too!
         assert_eq!(atom("false"),Ok(("",Box::new(Node::Boolean(false)))));
     }
     #[test]
     fn test_bool_true() {
         assert_eq!(atom("     \t\n\rtrue"), Ok(("",Box::new(Node::Boolean(true)))));
     }
 
     #[test]
     fn test_bool_false() {
         // This assert would fire and test will fail.
         // Please note, that private functions can be tested too!
         assert_eq!(atom("     \t\n\rfalse"),Ok(("",Box::new(Node::Boolean(false)))));
     }
 
     #[test]
     fn test_int_1234567_tight() {
         assert_eq!(atom("1234567"), Ok(("",Box::new(Node::Integer(1234567)))));
     }
 
     #[test]
     fn test_bool_2343_tight() {
         // This assert would fire and test will fail.
         // Please note, that private functions can be tested too!
         assert_eq!(atom("2343"),Ok(("",Box::new(Node::Integer(2343)))));
     }
 
     #[test]
     fn test_int_1234567() {
         assert_eq!(atom("     \t\n\r1234567"), Ok(("",Box::new(Node::Integer(1234567)))));
     }
 
     #[test]
     fn test_int_2343() {
         // This assert would fire and test will fail.
         // Please note, that private functions can be tested too!
         assert_eq!(atom("     \t\n\r2343"),Ok(("",Box::new(Node::Integer(2343)))));
     }

     #[test]
     fn test_ident_class_name() {
         assert_eq!(atom("     \t\n\rClassName"), Ok(("",Box::new(Node::Ident(String::from("ClassName"))))));
     }
 
     #[test]
     fn test_int_var_name() {
         // This assert would fire and test will fail.
         // Please note, that private functions can be tested too!
         assert_eq!(atom("     \t\n\rvar_name_1234"),Ok(("",Box::new(Node::Ident(String::from("var_name_1234"))))));
     }
 }
 