#[macro_use]
extern crate terms;
#[macro_use]
extern crate tree_automata as ta;
extern crate automatic_relations as automatic;

use std::cell::Cell;
use std::cmp::Ord;
use std::fmt;
use std::collections::HashSet;
use terms::{Term, Pattern, Var, variable::Spawnable};
use ta::{
    Symbol,
    State,
    Sorted,
    NoLabel,
    Rank,
    Ranked,
    bottom_up::{
        Automaton,
        Configuration,
        // LanguageState,
        width_search::TermFragment
    }
};

use automatic::{
    Relation,
    Convolution,
    Convoluted,
    MaybeBottom,
    convolution::aligned
};

struct PList<'a, T: fmt::Display>(&'a [T]);

impl<'a, T: fmt::Display> fmt::Display for PList<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.split_first() {
            Some((head, tail)) => {
                head.fmt(f)?;
                for e in tail.iter() {
                    write!(f, ",")?;
                    e.fmt(f)?;
                }
                Ok(())
            },
            None => Ok(())
        }
    }
}

fn assert_multi_search<F: Ranked + Symbol + Ord, Q: State, X: Spawnable>(automata: &[&Automaton<Rank<Convoluted<F>>, Q, NoLabel>], mut patterns: Vec<Vec<Pattern<F, X>>>, mut expected_output: Vec<Vec<Term<Rank<Convoluted<F>>>>>) {
    // prepare the patterns.
    let convoluted_patterns = patterns.drain(..).map(|patterns| {
        Convoluted(patterns.into_iter().map(|p| MaybeBottom::Some(p)).collect())
    }).collect();

    let it = aligned::multi_search(automata, convoluted_patterns);

    let mut output: Vec<Vec<Term<Rank<Convoluted<F>>>>> = Vec::with_capacity(expected_output.len());
    for terms in it {
        #[cfg(debug_assertions)]
        {
            if output.len() >= expected_output.len() {
                for terms in output.iter() {
                    println!("FOUND {}", PList(terms));
                }
                println!("FOUND {}", PList(&terms));
            }
        }
        assert!(output.len() < expected_output.len());
        output.push(terms)
    }

    let mut expected_output_set = HashSet::new();
    for e in expected_output.drain(..) {
        expected_output_set.insert(e);
    }

    #[cfg(debug_assertions)]
    {
        if output.len() != expected_output_set.len() {
            println!("EXPECTED OUTPUT:");
            for terms in expected_output_set.iter() {
                println!("{}", PList(terms));
            }
            println!("\nOUTPUT:");
            for terms in output.iter() {
                println!("{}", PList(terms));
            }
        }
    }

    assert!(output.len() == expected_output_set.len());

    for terms in output.iter() {
        #[cfg(debug_assertions)]
        {
            if !expected_output_set.contains(terms) {
                println!("UNEXPECTED TERM: {}", PList(terms));
            }
        }

        assert!(expected_output_set.contains(terms));
    }
}

/// In this test we have one relation P1 recognizing s^{2n+2}(0) and one relation P2 recognizing
/// s^{3n+3}(0). The goal is to find x such that P1(x) /\ P2(x). Since we search non-looping
/// solutions, the only expected answer is x = s(s(s(s(s(s(0)))))).
#[test]
fn mod_2_3() {
    let s = Rank("s", 1);
    let zero = Rank("0", 0);

    let s_conv = Rank(Convoluted(vec![MaybeBottom::Some(s)]), 1);
    let zero_conv = Rank(Convoluted(vec![MaybeBottom::Some(zero)]), 0);

    let mod2_2 = "mod2+2";
    let mod2_1 = "mod2+1";
    let mod2 = "mod2";

    let mod3_3 = "mod3+3";
    let mod3_2 = "mod3+2";
    let mod3_1 = "mod3+1";
    let mod3 = "mod3";

    let mut prop_mod2 = automaton! {
        zero_conv -> mod2,
        s_conv(mod2_1) -> mod2,
        s_conv(mod2) -> mod2_1,

        s_conv(mod2_1) -> mod2_2,

        finals mod2_2
    };

    let mut prop_mod3 = automaton! {
        zero_conv -> mod3,
        s_conv(mod3_2) -> mod3,
        s_conv(mod3_1) -> mod3_2,
        s_conv(mod3) -> mod3_1,

        s_conv(mod3_2) -> mod3_3,

        finals mod3_3
    };

    let namespace = Cell::new(0);
    let x = Var::spawn(&&namespace);

    assert_multi_search(
        &[&prop_mod2, &prop_mod3],
        vec![ vec![ pattern![ ?x ] ], vec! [ pattern![ ?x ] ] ],
        vec![
            vec![
                aligned::Convolution::convolute(vec![ Some(term![ s(s(s(s(s(s(zero)))))) ]) ]),
                aligned::Convolution::convolute(vec![ Some(term![ s(s(s(s(s(s(zero)))))) ]) ])
            ]
        ]
    )
}

/// In this test we have one relation P1 recognizing s^{2n+2}(0) and one relation P2 recognizing
/// s^{3n+3}(0). The goal is to find x such that P1(x) /\ P2(x). Since we search non-looping
/// solutions, the only expected answer is x = s(s(s(s(s(s(0)))))).
#[test]
fn even1() {
    let s = Rank("s", 1);
    let zero = Rank("0", 0);

    let s_conv = Rank(Convoluted(vec![MaybeBottom::Some(s)]), 1);
    let zero_conv = Rank(Convoluted(vec![MaybeBottom::Some(zero)]), 0);

    let nat = "Nat";

    let mut aut = automaton! {
        zero_conv -> nat,
        s_conv(nat) -> nat,

        finals nat
    };

    let namespace = Cell::new(0);
    let x = Var::spawn(&&namespace);

    assert_multi_search::<_, _, Var<usize>>(
        &[&aut],
        vec![ vec![ pattern![ zero ] ] ],
        vec![
            vec![
                aligned::Convolution::convolute(vec![ Some(term![ zero ]) ])
            ]
        ]
    )
}

/// This test is intersting because the loop detection algorithm must take into account that the
/// first occurance of x is constrained in a pattern (cons(x, b)).
#[test]
fn constrained_loop() {
    let a = Rank("a", 0);
    let b = Rank("b", 0);

    let nil = Rank("nil", 0);
    let cons = Rank("cons", 2);

    let a_conv = Rank(Convoluted(vec![MaybeBottom::Some(a)]), 0);
    let b_conv = Rank(Convoluted(vec![MaybeBottom::Some(b)]), 0);
    let nil_conv = Rank(Convoluted(vec![MaybeBottom::Some(nil)]), 0);
    let cons_conv = Rank(Convoluted(vec![MaybeBottom::Some(cons)]), 2);

    let mut prop_all_false = automaton! {
        b_conv -> "0:Bool",
        cons_conv("0:List", "0:Bool") -> "0:List",
        nil_conv -> "0:List",

        finals "0:List"
    };

    let mut prop_sorted = automaton! {
        cons_conv("_:List", "_:Bool") -> "_:List",
        cons_conv("1:List", "_:Bool") -> "_:List",
        cons_conv("2:List", "0:Bool") -> "_:List",
        cons_conv("_:List", "1:Bool") -> "_:List",
        cons_conv("_:List", "0:Bool") -> "_:List",
        cons_conv("0:List", "_:Bool") -> "_:List",
        cons_conv("2:List", "1:Bool") -> "_:List",
        cons_conv("2:List", "_:Bool") -> "_:List",
        cons_conv("1:List", "0:Bool") -> "2:List",
        a_conv -> "1:Bool",
        nil_conv -> "0:List",
        cons_conv("0:List", "0:Bool") -> "1:List",
        cons_conv("0:List", "1:Bool") -> "1:List",
        cons_conv("1:List", "1:Bool") -> "1:List",
        b_conv -> "0:Bool",

        finals "_:List" "2:List"
    };

    let namespace = Cell::new(0);
    let x = Var::spawn(&&namespace);

    assert_multi_search(
        &[&prop_all_false, &prop_sorted],
        vec![ vec![ pattern![ ?x ] ], vec! [ pattern![ cons(?x, b) ] ] ],
        vec![
            vec![
                aligned::Convolution::convolute(vec![ Some(term![ cons(nil, b) ]) ]),
                aligned::Convolution::convolute(vec![ Some(term![ cons(cons(nil, b), b) ]) ])
            ]
        ]
    )
}
