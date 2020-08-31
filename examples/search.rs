#![feature(unsized_tuple_coercion)]

#[macro_use]
extern crate terms;
#[macro_use]
extern crate tree_automata as ta;
extern crate automatic_relations as automatic;

use std::cell::Cell;
use std::cmp::Ord;
use std::collections::HashSet;
use terms::{
	Term,
	Pattern,
	Var,
	variable::{
		Spawnable,
		Family,
		Parented
	}
};
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

fn assert_search<F: Ranked + Symbol + Ord, Q: State, X: Family + Ord>(aut: &Automaton<Rank<Convoluted<F>>, Q, NoLabel>, patterns: Vec<Pattern<F, X>>, mut expected_output: Vec<Term<Rank<Convoluted<F>>>>) {
    // prepare the pattern.
    let convoluted_pattern = Convoluted(patterns.into_iter().map(|p| MaybeBottom::Some(p)).collect());

    let it = aligned::search(aut, vec![convoluted_pattern], None);

    let mut output = Vec::with_capacity(expected_output.len());
    for terms in it {
		let terms = terms.unwrap();

        for term in terms {
            #[cfg(debug_assertions)]
            {
                if output.len() >= expected_output.len() {
                    for term in output.iter() {
                        println!("FOUND {}", term);
                    }
                    println!("FOUND {}", term);
                }
            }
            assert!(output.len() < expected_output.len());
            output.push(term)
        }
    }

    output.sort();

    let mut expected_output_set = HashSet::new();
    for e in expected_output.drain(..) {
        expected_output_set.insert(e);
    }

    #[cfg(debug_assertions)]
    {
        if output.len() != expected_output_set.len() {
            println!("EXPECTED OUTPUT:");
            for e in expected_output_set.iter() {
                println!("{}", e);
            }
            println!("\nOUTPUT:");
            for e in output.iter() {
                println!("{}", e);
            }
        }
    }

    assert!(output.len() == expected_output_set.len());

    for e in output.iter() {
        assert!(expected_output_set.contains(e));
    }
}

fn main() {
    let s = Rank("s", 1);
    let zero = Rank("0", 0);
    let nat = "nat";

    let mut types = automaton! {
        s(nat) -> nat,
        zero -> nat,

        finals nat
    };

    // build the equality relation between 2 terms.
    let eq_types = aligned::Convolution::equality(&types, 2);

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &eq_types,
        vec![ pattern![ s(s(?x)) ], pattern![ ?y ] ],
        vec![
            aligned::Convolution::convolute(vec![ Some(term![ s(s(zero)) ]), Some(term![ s(s(zero)) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ s(s(s(zero))) ]), Some(term![ s(s(s(zero))) ]) ])
        ]
    )
}
