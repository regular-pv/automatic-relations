#![feature(trait_alias, generic_associated_types)]

extern crate terms;
extern crate tree_automata as ta;

pub mod pattern_eq;

use std::fmt;
use std::marker::PhantomData;
use terms::Term;
use ta::{Automaton, Language, Symbol, State, Label, Ranked};
use ta::search::Representatives;
use ta::combinations_option;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Convoluted<T>(Vec<Option<T>>);

impl<T: Ranked> Ranked for Convoluted<T> {
    fn arity(&self) -> usize {
        let mut arity = 0;
        for e in self.0.iter() {
            let e_arity = {
                match e {
                    Some(t) => t.arity(),
                    None => 0
                }
            };
            if e_arity > 0 {
                if arity > 0 {
                    arity *= e_arity
                } else {
                    arity = e_arity
                }
            }
        }
        arity
    }
}

impl<T: fmt::Display> fmt::Display for Convoluted<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.split_first() {
            Some((head, tail)) => {
                match head {
                    Some(e) => e.fmt(f)?,
                    None => write!(f, "⟘")?
                }

                for e in tail.iter() {
                    write!(f, "⊗")?;
                    match e {
                        Some(e) => e.fmt(f)?,
                        None => write!(f, "⟘")?
                    }
                }
                Ok(())
            },
            None => Ok(())
        }
    }
}

pub trait Relation<F: Ranked> {
    /// Check if the relation contains the given terms.
    /// The number of term must be equal to the arity of the relation.
    fn contains(&self, terms: &[&Term<F>]) -> bool;

    /// Iterate over the representatives members of the relation.
    fn related_representatives<'a>(&'a self) -> Box<dyn Iterator<Item = Vec<Term<F>>> + 'a>;
}

impl<F: Symbol + Ranked, Q: State, L: Label> Relation<F> for Automaton<Convoluted<F>, Q, L> {
    /// Check if the relation contains the given terms.
    /// The number of term must be equal to the arity of the relation.
    fn contains(&self, terms: &[&Term<F>]) -> bool {
        panic!("TODO")
    }

    /// Iterate over the representatives members of the relation.
    fn related_representatives<'a>(&'a self) -> Box<dyn Iterator<Item = Vec<Term<F>>> + 'a> {
        Box::new(RelatedRepresentatives::new(self))
    }
}

/// Will panic is one the term is bottom.
pub fn deconvolute<F: Symbol + Ranked>(convoluted: &Term<Convoluted<F>>) -> Option<Vec<Term<F>>> {
    match deconvolute_subterm(convoluted) {
        Some(mut deconvoluted) => {
            Some(deconvoluted.drain(..).map(|t| t.unwrap()).collect())
        },
        None => None
    }
}

pub fn deconvolute_subterm<F: Symbol + Ranked>(convoluted: &Term<Convoluted<F>>) -> Option<Vec<Option<Term<F>>>> {
    let symbols = &convoluted.symbol().0;
    let arity = convoluted.symbol().arity();
    if arity > 0 {
        let it = combinations_option(symbols, |f| {
            if let Some(f) = f {
                0..f.arity()
            } else {
                0..0
            }
        });

        let mut flat_terms: Vec<Vec<Option<Term<F>>>> = Vec::with_capacity(symbols.len());
        // "flat" because we just use it to store the subterms of each term, without the symbols.
        for f in symbols.iter() {
            if let Some(f) = f {
                let f_arity = f.arity();
                let mut subterms = Vec::with_capacity(f_arity);
                subterms.resize(f_arity, None);
                flat_terms.push(subterms)
            } else {
                flat_terms.push(Vec::new())
            }
        }

        for (k, positions) in it.enumerate() {
            // k is the position in the convoluted configuration.

            match deconvolute_subterm(&convoluted.sub_terms()[k]) {
                Some(deconvoluted_subterms) => {
                    let d: &Vec<Option<Term<F>>> = &deconvoluted_subterms;
                    for (i, pos) in positions.iter().enumerate() {
                        // `i` is the deconvoluted configuration number.
                        // `pos` is the position in the deconvoluted configuration.

                        if let Some(pos) = pos {
                            match flat_terms[i][*pos] {
                                Some(ref known_subterm) => {
                                    // check consistency
                                    match deconvoluted_subterms[i] {
                                        Some(ref subterm) => {
                                            if subterm != known_subterm {
                                                return None // malformed
                                            }
                                        },
                                        None => return None
                                    }
                                },
                                None => {
                                    flat_terms[i][*pos] = deconvoluted_subterms[i].clone()
                                }
                            }
                        } else {
                            if deconvoluted_subterms[i].is_some() {
                                return None // malformed
                            }
                        }
                    }
                },
                None => return None
            }
        }

        let mut terms = Vec::with_capacity(flat_terms.len());
        for (i, mut subterms_option) in flat_terms.drain(..).enumerate() {
            match &symbols[i] {
                Some(f) => {
                    let mut subterms = Vec::with_capacity(subterms_option.len());
                    for sub_opt in subterms_option.drain(..) {
                        if let Some(sub) = sub_opt {
                            subterms.push(sub)
                        } else {
                            panic!("malformed convoluted term")
                        }
                    }

                    terms.push(Some(Term::new(f.clone(), subterms)))
                },
                None => {
                    terms.push(None)
                }
            }
        }

        Some(terms)
    } else {
        let mut terms = Vec::with_capacity(symbols.len());
        for f in symbols.iter() {
            if let Some(f) = f {
                terms.push(Some(Term::new(f.clone(), Vec::new())))
            } else {
                terms.push(None)
            }
        }

        Some(terms)
    }
}

pub struct RelatedRepresentatives<'a, F: Symbol + Ranked, Q: State, L: Label> {
    it: Representatives<'a, Convoluted<F>, Q, L>
}

impl<'a, F: Symbol + Ranked, Q: State, L: Label> RelatedRepresentatives<'a, F, Q, L> {
    fn new(automaton: &'a Automaton<Convoluted<F>, Q, L>) -> RelatedRepresentatives<'a, F, Q, L> {
        RelatedRepresentatives {
            it: automaton.representatives()
        }
    }
}

impl<'a, F: Symbol + Ranked, Q: State, L: Label> Iterator for RelatedRepresentatives<'a, F, Q, L> {
    type Item = Vec<Term<F>>;

    fn next(&mut self) -> Option<Vec<Term<F>>> {
        loop {
            match self.it.next() {
                Some(convoluted) => {
                    if let Some(terms) = deconvolute(&convoluted) {
                        return Some(terms)
                    }
                },
                None => return None
            }
        }
    }
}
