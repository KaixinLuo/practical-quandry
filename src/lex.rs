
use nom::IResult;
use nom::bytes::complete::tag;
use crate::node::Node;

type NomCombType = dyn FnMut(&str)->IResult<&str,Box<Node>>;