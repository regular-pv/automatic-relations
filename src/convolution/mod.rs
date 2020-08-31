use terms::{
    Term,
    Pattern,
    variable::Family
};
use ta::{
    Symbol,
    State,
    NoLabel,
    Rank,
    bottom_up::{
        Automaton,
        width_search::{LanguageState, Killed}
    }
};
use crate::{
    Convoluted
};

pub mod aligned;

/// Convolution operations.
pub trait Convolution<F: Symbol> {
    /// Perform a term convolution.
    fn convolute<T: AsRef<[Option<Term<F>>]>>(terms: T) -> Term<Rank<Convoluted<F>>>;

    // /// Perform a pattern convolution.
    // fn convolute_patterns<X: Clone, P: AsRef<[Option<Pattern<F, X>>]>>(patterns: P) -> ConvolutedPattern<F, X>;

    /// Perform a term deconvolution.
    fn deconvolute(term: Term<Rank<Convoluted<F>>>) -> Vec<Option<Term<F>>>;

    /// Generic alternating-automaton out of a convoluted bottom-up automaton.
    fn generic_automaton<Q: State>(aut: &Automaton<Rank<Convoluted<F>>, Q, NoLabel>) -> ta::alternating::Automaton<Convoluted<F>, Q, Convoluted<u32>>;

    /// Compute the equality predicate over a domain between `n` variables.
    fn equality<Q: State>(aut: &Automaton<F, Q, NoLabel>, n: usize) -> Automaton<Rank<Convoluted<F>>, Q, NoLabel>;

    // /// Compute the natural comparison operator `<` for the given nat constructors.
    // fn nat_less<Q: State>(s: F, zero: F) -> Automaton<Rank<Convoluted<F>>, Q, NoLabel>;
    //
    // /// Compute the natural comparison operator `<=` for the given nat constructors.
    // fn nat_leq<Q: State>(s: F, zero: F) -> Automaton<Rank<Convoluted<F>>, Q, NoLabel>;

    fn state_convolution<E, Q: LanguageState<F, E>>(initial_state: Convoluted<Q>, env: &E) -> Automaton<Rank<Convoluted<F>>, Convoluted<Q>, NoLabel>;

    fn search<'a, 'e, Q: State, X: 'a + Family + Ord + Clone>(automata: &'a [&'e Automaton<Rank<Convoluted<F>>, Q, NoLabel>], patterns: Vec<Convoluted<Pattern<F, X>>>, kill_signal: Option<crossbeam_channel::Receiver<()>>) -> Box<dyn Iterator<Item = Result<Vec<Term<Rank<Convoluted<F>>>>, Killed>> + 'a>;
}
