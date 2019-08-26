#![feature(trait_alias)]

extern crate terms;
extern crate tree_automata as ta;

use std::ops::Index;
use std::cmp::{PartialOrd, Ord, Ordering};
use std::slice::SliceIndex;
use std::fmt;
use terms::Term;
use ta::Ranked;

pub mod convolution;
pub mod pattern;

pub use convolution::Convolution;
pub use pattern::ConvolutedPattern;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Convoluted<T>(pub Vec<MaybeBottom<T>>);

impl<T> Convoluted<T> {
    pub fn iter(&self) -> std::slice::Iter<MaybeBottom<T>> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn signature(&self) -> u32 {
        let mut signature: u32 = 0;
        for t in &self.0 {
            signature <<= 1;
            if let MaybeBottom::Some(_) = t {
                signature |= 1;
            }
        }

        signature
    }
}

impl<T, I> Index<I> for Convoluted<T> where I: SliceIndex<[MaybeBottom<T>]> {
    type Output = I::Output;

    fn index(&self, i: I) -> &I::Output {
        self.0.index(i)
    }
}

impl<T: Ord> Ord for Convoluted<T> {
    fn cmp(&self, other: &Convoluted<T>) -> Ordering {
        match self.0.len().cmp(&other.0.len()) {
            Ordering::Equal => {
                for (i, a) in self.0.iter().enumerate() {
                    let b = &other.0[i];
                    match a.cmp(b) {
                        Ordering::Equal => (),
                        ord => return ord
                    }
                }

                Ordering::Equal
            },
            ord => ord
        }
    }
}

impl<T: Ord> PartialOrd for Convoluted<T> {
    fn partial_cmp(&self, other: &Convoluted<T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MaybeBottom<T> {
    Bottom,
    Some(T)
}

impl<T: PartialEq> PartialEq<T> for MaybeBottom<T> {
    fn eq(&self, other: &T) -> bool {
        match self {
            MaybeBottom::Some(t) => t == other,
            _ => false
        }
    }
}

impl<T: Ord> Ord for MaybeBottom<T> {
    fn cmp(&self, other: &MaybeBottom<T>) -> Ordering {
        match (self, other) {
            (MaybeBottom::Some(a), MaybeBottom::Some(b)) => a.cmp(b),
            (MaybeBottom::Some(_), _) => Ordering::Greater,
            (MaybeBottom::Bottom, MaybeBottom::Bottom) => Ordering::Equal,
            _ => Ordering::Less
        }
    }
}

impl<T: Ord> PartialOrd for MaybeBottom<T> {
    fn partial_cmp(&self, other: &MaybeBottom<T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub trait Relation<F: Ranked> {
    /// Check if the relation contains the given terms.
    /// The number of term must be equal to the arity of the relation.
    fn contains(&self, terms: &[&Term<F>]) -> bool;
}

impl<T: fmt::Display> fmt::Display for MaybeBottom<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MaybeBottom::Bottom => write!(f, "⟘"),
            MaybeBottom::Some(t) => t.fmt(f)
        }
    }
}

impl<T: fmt::Display> fmt::Display for Convoluted<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.split_first() {
            Some((head, tail)) => {
                head.fmt(f)?;
                for e in tail.iter() {
                    write!(f, "⊗")?;
                    e.fmt(f)?;
                }
                Ok(())
            },
            None => Ok(())
        }
    }
}
