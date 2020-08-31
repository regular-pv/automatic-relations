use terms::{
    Term,
    Pattern,
    variable::Family
};
use ta::{
    Symbol,
    State,
    NoLabel,
    Ranked,
    Rank,
    bottom_up::{
        Automaton,
        Configuration,
        width_search::{LanguageState, Killed}
    }
};
use crate::{
    Convoluted,
    MaybeBottom
};

pub mod automaton;
mod search;
pub use search::{search, multi_search};

///
pub struct Convolution {
    // ...
}

// /// Indexed symbols.
// pub trait Indexed: Symbol + Ranked {
//     /// Get the index of the n-th position.
//     fn index(n: usize) -> usize;
// }

pub struct ConvolutedPatternBuilder<F: Symbol, X: Clone> {
    sub_patterns_to_convolute: Vec<Convoluted<Pattern<F, X>>>,
    arity: usize
}

impl<F: Symbol, X: Clone> ConvolutedPatternBuilder<F, X> {
    pub fn new() -> ConvolutedPatternBuilder<F, X> {
        ConvolutedPatternBuilder {
            sub_patterns_to_convolute: Vec::new(),
            arity: 0
        }
    }

    /// Set the sub-pattern i of the pattern k.
    pub fn set(&mut self, k: usize, i: usize, pattern: Pattern<F, X>) {
        if self.sub_patterns_to_convolute.len() <= i {
            let mut list = Vec::new();
            list.resize(self.arity+1, MaybeBottom::Bottom);
            self.sub_patterns_to_convolute.resize(i+1, Convoluted(list));
        }

        if self.arity <= k {
            for sub_pattern in self.sub_patterns_to_convolute.iter_mut() {
                sub_pattern.0.resize(k+1, MaybeBottom::Bottom);
            }

            self.arity = k;
        }

        self.sub_patterns_to_convolute[i].0[k] = MaybeBottom::Some(pattern)
    }

    pub fn unwrap(self) -> Vec<Convoluted<Pattern<F, X>>> {
        self.sub_patterns_to_convolute
    }
}

impl<F: Symbol + Ranked> crate::Convolution<F> for Convolution {
    /// Perform a term convolution.
    fn convolute<T: AsRef<[Option<Term<F>>]>>(terms: T) -> Term<Rank<Convoluted<F>>> {
        let terms = terms.as_ref();

        let mut convoluted_symbol = Vec::with_capacity(terms.len());
        let mut sub_terms_to_convolute = Vec::new();

        for (k, term) in terms.iter().enumerate() {
            if let Some(term) = term {
                convoluted_symbol.push(MaybeBottom::Some(term.symbol().clone()));

                for (i, sub_term) in term.sub_terms().iter().enumerate() {
                    if sub_terms_to_convolute.len() <= i {
                        let mut vec = Vec::with_capacity(k);
                        vec.resize(k, None);
                        sub_terms_to_convolute.resize(i+1, vec);
                    }
                    sub_terms_to_convolute[i].push(Some(sub_term.clone()));
                }

                for i in term.sub_terms().len()..sub_terms_to_convolute.len() {
                    sub_terms_to_convolute[i].push(None);
                }
            } else {
                convoluted_symbol.push(MaybeBottom::Bottom);
                for i in 0..sub_terms_to_convolute.len() {
                    sub_terms_to_convolute[i].push(None);
                }
            }
        }

        let convoluted_sub_terms = sub_terms_to_convolute.iter().map(|sub_terms| {
            Self::convolute(&sub_terms)
        }).collect();

        Term::new(Rank(Convoluted(convoluted_symbol), sub_terms_to_convolute.len()), convoluted_sub_terms)
    }

    /// Perform a term deconvolution.
    fn deconvolute(_term: Term<Rank<Convoluted<F>>>) -> Vec<Option<Term<F>>> {
        panic!("TODO deconvolution");
    }

    /// Create the equality relation between n terms.
    fn equality<Q: State>(aut: &Automaton<F, Q, NoLabel>, n: usize) -> Automaton<Rank<Convoluted<F>>, Q, NoLabel> {
        let mut eq_aut = Automaton::new();

        for (Configuration(f, sub_states), _, q) in aut.transitions() {
            let mut convoluted_f = Vec::with_capacity(n);
            convoluted_f.resize(n, MaybeBottom::Some(f.clone()));

            eq_aut.add(Configuration(Rank(Convoluted(convoluted_f), sub_states.len()), sub_states.clone()), NoLabel, q.clone());
        }

        for final_q in aut.final_states() {
            eq_aut.set_final(final_q.clone());
        }

        eq_aut
    }

    // fn nat_less<Q: State>(s: F, zero: F) -> Automaton<Rank<Convoluted<F>>, Q, NoLabel> {
    //     let mut eq_aut = Automaton::new();
    //
    //     // ...
    // }

    fn generic_automaton<Q: State>(aut: &Automaton<Rank<Convoluted<F>>, Q, NoLabel>) -> ta::alternating::Automaton<Convoluted<F>, Q, Convoluted<u32>> {
        let mut alt = ta::alternating::Automaton::new();

        for (Configuration(convoluted_f, subs), _, convoluted_q) in aut.transitions() {
            let conjunction = subs.iter().enumerate().map(|(i, convoluted_sub)| {
                let indexes = convoluted_f.0.iter().map(|f| {
                    if let MaybeBottom::Some(f) = f {
                        if i < f.arity() {
                            MaybeBottom::Some(i as u32)
                        } else {
                            MaybeBottom::Bottom
                        }
                    } else {
                        MaybeBottom::Bottom
                    }
                }).collect();
                (Convoluted(indexes), convoluted_sub.clone())
            }).collect();
            alt.add(convoluted_q, &convoluted_f.0, conjunction);
        }

        for convoluted_q in aut.final_states() {
            alt.set_initial(convoluted_q.clone());
        }

        alt
    }

    fn state_convolution<E, Q: LanguageState<F, E>>(initial_state: Convoluted<Q>, env: &E) -> Automaton<Rank<Convoluted<F>>, Convoluted<Q>, NoLabel> {
        automaton::state_convolution(initial_state, env)
    }

    fn search<'a, 'e, Q: State, X: 'a + Family + Ord>(automata: &'a [&'e Automaton<Rank<Convoluted<F>>, Q, NoLabel>], patterns: Vec<Convoluted<Pattern<F, X>>>, kill_signal: Option<crossbeam_channel::Receiver<()>>) -> Box<dyn Iterator<Item = Result<Vec<Term<Rank<Convoluted<F>>>>, Killed>> + 'a> {
        Box::new(multi_search(automata, patterns, kill_signal))
    }
}

//

// #[derive(Clone, Debug)]
// pub struct Convoluted<Pattern<F, X>>(pub Vec<MaybeBottom<Pattern<F, X>>>);
//
// impl<F: fmt::Display, X: fmt::Display> fmt::Display for Convoluted<Pattern<F, X>> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self.0.split_first() {
//             Some((head, tail)) => {
//                 head.fmt(f)?;
//                 for e in tail.iter() {
//                     write!(f, "⊗{}", e)?;
//                 }
//
//                 Ok(())
//             },
//             None => Ok(())
//         }
//     }
// }
