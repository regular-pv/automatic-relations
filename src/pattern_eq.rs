// pub enum PatternEq<'a, F, Q, X> {
//     Cons {
//         f: &'a F,
//         states: Vec<PatternEq<'a, F, Q, X>>,
//         padding: Vec<Position<Q>>
//     }
// }
//
// /// Find a
// impl PatternEq {
//     pub fn new(pattern: &Pattern<F, Q, X>, variables: &[X], padding: Vec<Position<Q>>]) -> Option<Relation<Convoluted<F>>> {
//         // ...
//         match pattern {
//             Pattern::Var(x) => {
//                 let index = variables.index_of(x);
//                 let it = Equality::new(index, x.sort(), padding);
//                 PatternEq::Var {
//                     iterator: Equality::new(index)
//                 }
//             },
//             Pattern::Cons(f, sub_patterns) => {
//                 let new_padding = padding.iter().map(|p| p.dive()).collect();
//                 PatternEq::Cons {
//                     f: f,
//                     states: sub_patterns.iter().map(|sub| PatternEq::new(sub, variables, new_padding)),
//                     padding: padding,
//                     // sub_patterns: sub_patterns,
//                     // padding: ,
//                     // iterators: sub_patterns.iter().map(|sub_pattern| PatternEq::new(sub_pattern, variables, new_padding))
//                     symbols:
//                 }
//             }
//         }
//     }
//
//     pub fn configurations() {
//         match self {
//             PatternEq::Var { x } => {
//                 let index = variables.index_of(x);
//                 PatternEqConfigurations::Var {
//                     x: x,
//                     index: index,
//                     symbols: combinations(padding, { |p| p.configurations() }),
//                     current_symbol: None,
//                     iterator: None
//                 }
//             }
//             PatternEq::Cons { f, states, padding } => {
//                 PatternEqConfigurations::Cons {
//                     f: f,
//                     states: states,
//                     padding: padding,
//                     symbols: combinations(padding, { |p| p.configurations() })
//                 }
//             }
//         }
//     }
// }
//
// pub enum PatternEqConfigurations {
//     Var {
//         sort: Q,
//         index: usize,
//         states: &'a Vec<PatternEq<'a, F, Q, X>>,
//         symbols: Box<Iterator<Item = Vec<Padded<F>>>>,
//         current_symbol: Option<Vec<Padded<F>>>,
//         iterator: Option<F>
//     },
//     Cons {
//         f: &'a F,
//         states: &'a Vec<PatternEq<'a, F, Q, X>>,
//         padding: &'a Vec<Position<Q>>,
//         symbols: Box<Iterator<Item = Vec<Padded<F>>>>
//     }
// }
//
// impl PatternEqConfigurations {
//     fn produce(f: &'a F, g: &'a Vec<Padded<F>>, states: &'a Vec<PatternEq<'a, F, Q, X>>) {
//         let convoluted_symbol = Vec::with_capacity(g.len()+1);
//         convoluted_symbol.append(g);
//         convoluted_symbol.push(Padded::new(f));
//
//         let arity = convoluted_symbol.arity();
//         let states = Vec::with_capacity(arity);
//         for i in 0..arity {
//             let sub_pattern = sub_patterns[f.index(i)];
//             let padding = Vec::with_capacity(g.len());
//             for gk in g.iter() {
//                 let j = gk.index(i); // TODO
//             }
//         }
//
//         let states = states.clone();
//         if g.arity() > f.arity() {
//             // TODO ...
//         } else {
//             // TODO ...
//         }
//
//         Configuration(convoluted_symbol, states)
//     }
// }
//
// impl Iterator for PatternEqConfigurations {
//     fn next() {
//         match self {
//             PatternEq::Var { sort, index, symbols, current_symbol, iterator } => {
//                 loop {
//                     match current_symbol {
//                         Some(g) => {
//                             match iterator.unwrap().next() {
//                                 Some(f) => {
//                                     if g[index] == f { // check equality
//                                         Some(PatternEqConfigurations::produce(g, f, states))
//                                     }
//                                 },
//                                 None => {
//                                     *current_symbol = None
//                                 }
//                             }
//                         },
//                         None => {
//                             match symbols.next() {
//                                 Some(g) => {
//                                     *current_symbol = Some(g);
//                                     *iterator = Some(sort.configurations())
//                                 },
//                                 None => return None
//                             }
//                         }
//                     }
//                 }
//             },
//             PatternEq::Cons { f, states, mut symbols } => {
//                 match symbols.next() {
//                     Some(g) => {
//                         Some(PatternEqConfigurations::produce(g, f, states))
//                     },
//                     None => {
//                         None
//                     }
//                 }
//             }
//         }
//     }
// }
