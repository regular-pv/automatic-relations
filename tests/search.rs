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
            // #[cfg(debug_assertions)]
            // {
            //     if output.len() >= expected_output.len() {
            //         for term in output.iter() {
            //             println!("FOUND {}", term);
            //         }
            //         println!("FOUND {}", term);
            //     }
            // }
            // assert!(output.len() < expected_output.len());
            output.push(term)
        }
    }

    output.sort();

    let mut expected_output_set = HashSet::new();
    for e in expected_output.drain(..) {
        expected_output_set.insert(e);
    }

    // #[cfg(debug_assertions)]
    // {
    //     if output.len() != expected_output_set.len() {
    //         println!("EXPECTED OUTPUT:");
    //         for e in expected_output_set.iter() {
    //             println!("{}", e);
    //         }
    //         println!("\nOUTPUT:");
    //         for e in output.iter() {
    //             println!("{}", e);
    //         }
    //     }
    // }

	for eo in &expected_output_set {
		#[cfg(debug_assertions)]
		if !output.contains(&eo) {
			println!("missing output: {}", eo);
		}
		assert!(output.contains(&eo));
	}

    // assert!(output.len() == expected_output_set.len());
    // for e in output.iter() {
    //     assert!(expected_output_set.contains(e));
    // }
}

#[test]
fn linear1() {
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
            // aligned::Convolution::convolute(vec![ Some(term![ s(s(s(zero))) ]), Some(term![ s(s(s(zero))) ]) ]),
			// aligned::Convolution::convolute(vec![ Some(term![ s(s(s(s(zero)))) ]), Some(term![ s(s(s(s(zero)))) ]) ])
        ]
    )
}

#[test]
fn linear_impossible1() {
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
        vec![ pattern![ s(?x) ], pattern![ ?x ] ],
        vec![
            // empty
        ]
    )
}

#[test]
fn non_linear1() {
    let n = Rank("n", 2);
    let zero = Rank("0", 0);
    let tree = "tree";

    let mut types = automaton! {
        n(tree, tree) -> tree,
        zero -> tree,

        finals tree
    };

    // build the equality relation between 2 terms.
    let eq_types = aligned::Convolution::equality(&types, 2);

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &eq_types,
        vec![ pattern![ n(?x, ?x) ], pattern![ ?y ] ],
        vec![
            aligned::Convolution::convolute(vec![ Some(term![ n(zero, zero) ]), Some(term![ n(zero, zero) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(n(zero, zero), n(zero, zero)) ]), Some(term![ n(n(zero, zero), n(zero, zero)) ]) ]),
			// aligned::Convolution::convolute(vec![ Some(term![ n(n(zero, zero), n(zero, zero)) ]), Some(term![ n(n(zero, zero), n(zero, zero)) ]) ])
        ]
    )
}

#[test]
fn non_linear2() {
    let n = Rank("n", 2);
    let a = Rank("a", 0);
    // let b = Rank("b", 0);
    let tree = "tree";

    let mut types = automaton! {
        n(tree, tree) -> tree,
        a -> tree,
        // b -> tree,

        finals tree
    };

    // build the equality relation between 2 terms.
    let eq_types = aligned::Convolution::equality(&types, 2);

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &eq_types,
        vec![ pattern![ n(?x, a) ], pattern![ ?y ] ],
        vec![
            aligned::Convolution::convolute(vec![ Some(term![ n(a, a) ]), Some(term![ n(a, a) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ n(b, a) ]), Some(term![ n(b, a) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(n(a, a), a) ]), Some(term![ n(n(a, a), a) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ n(n(a, b), a) ]), Some(term![ n(n(a, b), a) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ n(n(b, a), a) ]), Some(term![ n(n(b, a), a) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ n(n(b, b), a) ]), Some(term![ n(n(b, b), a) ]) ])
        ]
    )
}

#[test]
fn non_linear3() {
    let n = Rank("n", 2);
    let a = Rank("a", 0);
    let b = Rank("b", 0);
    let tree = "tree";

    let mut types = automaton! {
        n(tree, tree) -> tree,
        a -> tree,
        b -> tree,

        finals tree
    };

    // build the equality relation between 2 terms.
    let eq_types = aligned::Convolution::equality(&types, 2);

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &eq_types,
        vec![ pattern![ n(?x, ?y) ], pattern![ n(?y, ?x) ] ],
        vec![
            aligned::Convolution::convolute(vec![ Some(term![ n(a, a) ]), Some(term![ n(a, a) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(b, b) ]), Some(term![ n(b, b) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(n(a, a), n(a, a)) ]), Some(term![ n(n(a, a), n(a, a)) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(n(a, b), n(a, b)) ]), Some(term![ n(n(a, b), n(a, b)) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(n(b, a), n(b, a)) ]), Some(term![ n(n(b, a), n(b, a)) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ n(n(b, b), n(b, b)) ]), Some(term![ n(n(b, b), n(b, b)) ]) ])
        ]
    )
}

#[test]
fn non_linear_impossible1() {
    let n = Rank("n", 2);
    let zero = Rank("0", 0);
    let tree = "tree";

    let mut types = automaton! {
        n(tree, tree) -> tree,
        zero -> tree,

        finals tree
    };

    // build the equality relation between 2 terms.
    let eq_types = aligned::Convolution::equality(&types, 2);

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &eq_types,
        vec![ pattern![ n(?x, ?x) ], pattern![ ?x ] ],
        vec![
            // empty
        ]
    )
}

#[test]
fn lists1() {
    let cons = Rank("cons", 2);
    let nil = Rank("nil", 0);
    let a = Rank("a", 0);
    let b = Rank("b", 0);
    let s = Rank("s", 1);
    let zero = Rank("0", 0);

    let cons_s = Rank(Convoluted(vec![MaybeBottom::Some(cons), MaybeBottom::Some(s)]), 2);
    let nil_zero = Rank(Convoluted(vec![MaybeBottom::Some(nil), MaybeBottom::Some(zero)]), 0);
    let a_bottom = Rank(Convoluted(vec![MaybeBottom::Some(a), MaybeBottom::Bottom]), 0);
    let b_bottom = Rank(Convoluted(vec![MaybeBottom::Some(b), MaybeBottom::Bottom]), 0);

    let length_eq = "=";
    let any = "*";

    let mut prop = automaton! {
        cons_s(length_eq, any) -> length_eq,
        nil_zero -> length_eq,
        a_bottom -> any,
        b_bottom -> any,

        finals length_eq
    };

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &prop,
        vec![ pattern![ ?x ], pattern![ ?y ] ],
        vec![
            aligned::Convolution::convolute(vec![ Some(term![ nil ]), Some(term![ zero ]) ])
        ]
    )
}

#[test]
fn lists2() {
    let cons = Rank("cons", 2);
    let nil = Rank("nil", 0);
    let a = Rank("a", 0);
    let b = Rank("b", 0);
    let s = Rank("s", 1);
    let zero = Rank("0", 0);

    let cons_s = Rank(Convoluted(vec![MaybeBottom::Some(cons), MaybeBottom::Some(s)]), 2);
    let nil_zero = Rank(Convoluted(vec![MaybeBottom::Some(nil), MaybeBottom::Some(zero)]), 0);
    let a_bottom = Rank(Convoluted(vec![MaybeBottom::Some(a), MaybeBottom::Bottom]), 0);
    let b_bottom = Rank(Convoluted(vec![MaybeBottom::Some(b), MaybeBottom::Bottom]), 0);

    let length_eq = "=";
    let any = "*";

    let mut prop = automaton! {
        cons_s(length_eq, any) -> length_eq,
        nil_zero -> length_eq,
        a_bottom -> any,
        b_bottom -> any,

        finals length_eq
    };

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);
    let y: Parented<Var<_>> = Parented::spawn(&&namespace);
    let z: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &prop,
        vec![ pattern![ cons(?x, ?z) ], pattern![ ?y ] ],
        vec![
            aligned::Convolution::convolute(vec![ Some(term![ cons(nil, a) ]), Some(term![ s(zero) ]) ]),
            aligned::Convolution::convolute(vec![ Some(term![ cons(nil, b) ]), Some(term![ s(zero) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ cons(cons(nil, a), a) ]), Some(term![ s(s(zero)) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ cons(cons(nil, b), a) ]), Some(term![ s(s(zero)) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ cons(cons(nil, a), b) ]), Some(term![ s(s(zero)) ]) ]),
            // aligned::Convolution::convolute(vec![ Some(term![ cons(cons(nil, b), b) ]), Some(term![ s(s(zero)) ]) ])
        ]
    )
}

#[test]
fn lists_impossible1() {
    let cons = Rank("cons", 2);
    let nil = Rank("nil", 0);
    let a = Rank("a", 0);
    let b = Rank("b", 0);
    let s = Rank("s", 1);
    let zero = Rank("0", 0);

    let cons_s = Rank(Convoluted(vec![MaybeBottom::Some(cons), MaybeBottom::Some(s)]), 2);
    let nil_zero = Rank(Convoluted(vec![MaybeBottom::Some(nil), MaybeBottom::Some(zero)]), 0);
    let a_bottom = Rank(Convoluted(vec![MaybeBottom::Some(a), MaybeBottom::Bottom]), 0);
    let b_bottom = Rank(Convoluted(vec![MaybeBottom::Some(b), MaybeBottom::Bottom]), 0);

    let length_eq = "=";
    let any = "*";

    let mut prop = automaton! {
        cons_s(length_eq, any) -> length_eq,
        nil_zero -> length_eq,
        a_bottom -> any,
        b_bottom -> any,

        finals length_eq
    };

    let namespace = Cell::new(0);
    let x: Parented<Var<_>> = Parented::spawn(&&namespace);

    assert_search(
        &prop,
        vec![ pattern![ nil ], pattern![ s(?x) ] ],
        vec![
            // empty
        ]
    )
}

/// In this test we generate a random finite relation and see if we can find every term of the
/// relation.
#[test]
fn finite1() {
    let alphabet = [
        Rank("c1", 1),
        Rank("c2", 2),
        Rank("c3", 3),
        Rank("c4", 4),
        Rank("a", 0),
        Rank("b", 0),
        Rank("c", 0),
        Rank("d", 0)
    ];

    let n = 3; // arity of the relation.
    let k = 3; // number of possible term in each position. The size of the relation will be k^n.
    let max_depth = 10;

    for _ in 0..10 {
        let mut terms = Vec::with_capacity(n);
        let mut patterns = Vec::with_capacity(n);
        let namespace = Cell::new(0);

        for _ in 0..n {
            let x: Parented<Var<_>> = Parented::spawn(&&namespace);
            patterns.push( pattern![ ?x ] );

            let mut vec = Vec::with_capacity(k);
            for _ in 0..k {
                vec.push(Term::random(&alphabet, 5));
            }

            terms.push(vec);
        }

        let mut prop = Automaton::new();
        let mut elements = Vec::with_capacity(k.pow(n as u32));
        let count = Cell::new(0);

        for element in ta::combinations(&terms, |t: &Vec<Term<Rank<&'static str>>>| t.iter()) {
            let element: Vec<Option<Term<_>>> = element.iter().map(|t| Some((*t).clone())).collect();
            let convoluted_element = aligned::Convolution::convolute(&element);
            let q = prop.add_normalized(&convoluted_element, &mut |_| {
                let q = count.get();
                count.set(q+1);
                (q, NoLabel)
            });
            prop.set_final(q);
            elements.push(convoluted_element);
        }

        assert_search(&prop, patterns, elements)
    }
}
