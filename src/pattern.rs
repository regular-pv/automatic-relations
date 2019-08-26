use std::fmt;
use terms::{
    Term
};
use crate::{
    Convoluted,
};

// pub struct SearchContext {
//     patterns: Vec<Convoluted<Pattern>>
// }

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MaybeVar<F, X> {
    Symbol(F),
    Var(X)
}

pub type ConvolutedPattern<F, X> = Term<Convoluted<MaybeVar<F, X>>>;

impl<F: fmt::Display, X: fmt::Display> fmt::Display for MaybeVar<F, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MaybeVar::Symbol(s) => s.fmt(f),
            MaybeVar::Var(t) => t.fmt(f)
        }
    }
}
