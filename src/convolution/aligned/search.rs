use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::cmp::Ord;
use std::rc::Rc;
use std::slice::Iter;
use once_cell::unsync::OnceCell;
use terms::{
	Pattern,
	PatternKind,
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
		Indexed,
		width_search::{
			SearchPattern,
			TermFragment
		}
	},
	Mux,
	combinations
};
use crate::{
	Convoluted,
	MaybeBottom,
};
use super::{
	ConvolutedPatternBuilder,
	// AlignedConvolutedPattern
};

type Renaming<X> = immutable_map::TreeMap<X, X>;

/// Convoluted pattern that must be recognized by the given state.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Atom<F: Symbol, X: Family + Ord + Clone, Q: State>(Convoluted<Pattern<F, X>>, Q);

impl<F: Symbol, X: Family + Ord + Clone, Q: State> Atom<F, X, Q> {
	fn matches(&self, other: &Atom<F, X, Q>, renaming: &Renaming<X>) -> Option<Renaming<X>> {
		if self.1 == other.1 {
			convoluted_pattern_renaming(&self.0, &other.0, renaming)
		} else {
			None
		}
	}

	fn variables(&self) -> HashSet<X> {
		let mut set = HashSet::new();

		for pattern in &self.0.0 {
			match pattern {
				MaybeBottom::Some(pattern) => set.extend(pattern.variables().cloned()),
				_ => ()
			}
		}

		set
	}

	fn instanciated(&self, map: &HashMap<X, Pattern<F, X>>) -> Atom<F, X, Q> {
		Atom(Convoluted(self.0.0.iter().map(|pattern| {
			match pattern {
				MaybeBottom::Some(pattern) => MaybeBottom::Some(pattern.map_variables(&|x| {
					match map.get(x) {
						Some(px) => px.clone(),
						None => Pattern::var(x.clone())
					}
				})),
				MaybeBottom::Bottom => MaybeBottom::Bottom
			}
		}).collect()), self.1.clone())
	}
}

impl<F: Symbol + fmt::Display, X: Family + Ord + Clone + fmt::Display, Q: State + fmt::Display> fmt::Display for Atom<F, X, Q> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		// for x in item.0.iter() {
		//	 if let Some(x) = x {
		//		 write!(f, "{}:", x)?;
		//	 } else {
		//		 write!(f, "_:")?;
		//	 }
		// }
		write!(f, "{}:{}", self.0, self.1)
	}
}

pub struct Atoms<'a, F: Symbol, X: Family + Ord + Clone, Q: State> {
	context: Option<&'a SearchContextData<F, Q, X>>,
	context_atoms: Iter<'a, Atom<F, X, Q>>,
	map: HashMap<X, Pattern<F, X>>
}

impl<'a, F: Symbol, X: Family + Ord + Clone, Q: State> Iterator for Atoms<'a, F, X, Q> {
	type Item = Atom<F, X, Q>;

	fn next(&mut self) -> Option<Atom<F, X, Q>> {
		loop {
			match self.context {
				Some(context) => {
					match self.context_atoms.next() {
						Some(atom) => return Some(atom.instanciated(&self.map)),
						None => {
							self.context = match &context.parent {
								Some(parent) if parent.depth == context.depth => {
									self.context_atoms = parent.atoms.iter();
									Some(parent.as_ref())
								},
								_ => None
							};
						}
					}
				},
				None => return None
			}
		}
	}
}

fn convoluted_pattern_renaming<F: PartialEq, X: Family + Ord + Clone>(a: &Convoluted<Pattern<F, X>>, b: &Convoluted<Pattern<F, X>>, renaming: &Renaming<X>) -> Option<Renaming<X>> {
	if a.0.len() == b.0.len() {
		let mut renaming = Renaming::clone(renaming);
		for i in 0..(a.0.len()) {
			match (&a[i], &b[i]) {
				(MaybeBottom::Some(a), MaybeBottom::Some(b)) => {
					match pattern_renaming(a, b, &renaming) {
						Some(r) => renaming = r,
						None => return None
					}
				},
				(MaybeBottom::Bottom, MaybeBottom::Bottom) => (),
				_ => return None
			}
		}

		Some(renaming)
	} else {
		None
	}
}

fn pattern_renaming<F: PartialEq, X: Family + Ord + Clone>(a: &Pattern<F, X>, b: &Pattern<F, X>, renaming: &Renaming<X>) -> Option<Renaming<X>> {
	match (a.kind(), b.kind()) {
		(PatternKind::Var(x), PatternKind::Var(y)) => {
			match renaming.get(x) {
				Some(z) => {
					if z == y {
						Some(Renaming::clone(renaming))
					} else {
						None
					}
				},
				None => {
					Some(renaming.insert(x.clone(), y.clone()))
				}
			}
		},
		(PatternKind::Cons(fa, la), PatternKind::Cons(fb, lb)) => {
			if fa == fb {
				let mut renaming = Renaming::clone(renaming);
				for i in 0..la.len() {
					match pattern_renaming(la.get(i).unwrap(), lb.get(i).unwrap(), &renaming) {
						Some(r) => renaming = r,
						None => return None
					}
				}

				Some(renaming)
			} else {
				None
			}
		},
		_ => None
	}
}

/// Sub-problem
///
/// The general search problem is broken down into these sub problems that can be used to
/// detect loops in the search.
///
/// Each sub-problem associates convoluted patterns to states.
/// Convoluted patterns in a given sub-problem are dependents, they share variables.
pub struct SubProblem<F: Symbol, X: Family + Ord + Clone, Q: State> {
	atoms: Vec<Atom<F, X, Q>>,
	variables: HashSet<X>
}

impl<F: Symbol + fmt::Display, X: Family + Ord + Clone + fmt::Display, Q: State + fmt::Display> fmt::Display for SubProblem<F, X, Q> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<")?;
		for item in self.atoms.iter() {
			write!(f, "({})", item)?;
		}
		write!(f, ">")
	}
}

impl<F: Symbol + fmt::Display, X: Family + Ord + Clone + fmt::Display, Q: State + fmt::Display> fmt::Debug for SubProblem<F, X, Q> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(self, f)
	}
}

struct IndexedAtom<X: Hash + Eq>(usize, HashSet<X>);

impl<F: Symbol, X: Family + Ord + Clone, Q: State> SubProblem<F, X, Q> {
	pub fn new() -> SubProblem<F, X, Q> {
		SubProblem {
			atoms: Vec::new(),
			variables: HashSet::new()
		}
	}

	pub fn singleton(atom: Atom<F, X, Q>, vars: HashSet<X>) -> SubProblem<F, X, Q> {
		SubProblem {
			atoms: vec![atom],
			variables: vars
		}
	}

	/// Make sure to also add the variables from the atom.
	fn add(&mut self, atom: Atom<F, X, Q>) -> bool {
		if !self.atoms.contains(&atom) {
			self.atoms.push(atom);
			true
		} else {
			false
		}
	}

	pub fn from_atoms<Atoms: Iterator<Item = Atom<F, X, Q>>>(atoms: Atoms) -> Vec<SubProblem<F, X, Q>> {
		// println!("== atoms:");
		use union_find::UnionFind;
		let mut uf: union_find::QuickFindUf<SubProblem<F, X, Q>> = union_find::QuickFindUf::new(8);
		let atoms: Vec<_> = atoms.filter_map(|atom| {
			let vars = atom.variables();
			if vars.is_empty() {
				None
			} else {
				// println!("{}", atom);
				let key = uf.insert(SubProblem::singleton(atom, vars.clone()));
				Some(IndexedAtom(key, vars))
			}
		}).collect();

		// println!("== sub problems:");

		for IndexedAtom(x, xvars) in &atoms {
			for IndexedAtom(y, yvars) in &atoms {
				if !xvars.is_disjoint(yvars) {
					uf.union(*x, *y);
				}
			}
		}

		let mut sub_problems = Vec::with_capacity(atoms.len());
		for IndexedAtom(i, _) in &atoms {
			let mut p = SubProblem::new();
			std::mem::swap(uf.get_mut(*i), &mut p);
			if !p.atoms.is_empty() {
				// println!("{}", p);
				sub_problems.push(p)
			}
		}

		sub_problems
	}

	fn is_descendent_of(&self, other: &SubProblem<F, X, Q>) -> bool {
		self.variables.iter().all(|x| other.variables.iter().any(|y| x.is_descendent_of(y)))
	}

	/// Checks if there exsits a renaming such that this sub-problem and the given one are equals.
	pub fn matches(&self, other: &SubProblem<F, X, Q>) -> bool {
		if self.atoms.len() == other.atoms.len() {
			let renaming = Renaming::new();
			let mut map = Vec::new();
			map.resize(self.atoms.len(), None);
			let r = self.find_renaming(&renaming, 0, &other.atoms, &mut map).is_some();
			if r {
				// println!("renaming between: {} and {}", self, other);
			} else {
				// println!("no renaming between: {} and {}", self, other);
			}
			r
		} else {
			false
		}
	}

	pub fn is_descendent_matching(&self, other: &SubProblem<F, X, Q>) -> bool {
		self.is_descendent_of(other) && self.matches(other)
	}

	pub fn find_renaming(&self, renaming: &Renaming<X>, i: usize, atoms: &Vec<Atom<F, X, Q>>, map: &mut Vec<Option<usize>>) -> Option<Renaming<X>> {
		if i >= self.atoms.len() {
			Some(renaming.clone())
		} else {
			let a = &self.atoms[i];

			for (j, b) in atoms.iter().enumerate() {
				if map[j].is_none() {
					if let Some(new_renaming) = a.matches(b, renaming) {
						map[j] = Some(i);

						if let Some(final_renaming) = self.find_renaming(&new_renaming, i+1, atoms, map) {
							return Some(final_renaming)
						}

						map[j] = None
					}
				}
			}

			None
		}
	}
}

impl<F: Symbol, X: Family + Ord + Clone, Q: State> union_find::Union for SubProblem<F, X, Q> {
	fn union(mut lval: Self, rval: Self) -> union_find::UnionResult<Self> {
		let ll = lval.atoms.len();
		let rl = rval.atoms.len();

		for atom in rval.atoms {
			lval.add(atom);
		}

		lval.variables.extend(rval.variables);

		if ll > rl {
			union_find::UnionResult::Left(lval)
		} else {
			union_find::UnionResult::Right(lval)
		}
	}
}

impl<F: Symbol, X: Family + Ord + Clone, Q: State> Default for SubProblem<F, X, Q> {
	fn default() -> Self {
		SubProblem::new()
	}
}

pub struct SearchContextData<F: Symbol, Q: State, X: Family + Ord + Clone> {
	depth: usize,
	parent: Option<Rc<SearchContextData<F, Q, X>>>,
	atoms: Vec<Atom<F, X, Q>>,
	bindings: HashMap<X, (F, Vec<X>)>,
	sub_problems: OnceCell<Vec<SubProblem<F, X, Q>>>
}

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

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Family + Ord + Clone + fmt::Display> fmt::Display for SearchContextData<F, Q, X> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for (x, (sym, vars)) in self.bindings.iter() {
			write!(f, "{}: {}({}), ", x, sym, PList(vars))?;
		}

		if let Some(parent) = &self.parent {
			write!(f, " < {}", parent)?;
		}

		Ok(())
	}
}

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Family + Ord + Clone + fmt::Display> fmt::Debug for SearchContextData<F, Q, X> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(self, f)
	}
}

#[derive(Clone)]
pub struct SearchContext<F: Symbol, Q: State, X: Family + Ord + Clone> {
	p: Rc<SearchContextData<F, Q, X>>
}

impl<F: Symbol, Q: State, X: Family + Ord + Clone> ta::bottom_up::width_search::SearchContext for SearchContext<F, Q, X> {
	fn looping(&self) -> bool {
		if self.p.sub_problems.get().is_some() {
			false
		} else {
			let sub_problems = SubProblem::from_atoms(self.p.depth_atoms());
			if self.p.looping_sub_problems(&sub_problems) {
				true
			} else {
				match self.p.sub_problems.set(sub_problems) {
					Ok(_) => false,
					Err(_) => unreachable!()
				}
			}
		}
	}
}

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Family + Ord + Clone + fmt::Display> fmt::Debug for SearchContext<F, Q, X> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(&self.p, f)
	}
}

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Family + Ord + Clone + fmt::Display> fmt::Display for SearchContext<F, Q, X> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(&self.p, f)
	}
}

impl<F: Symbol, Q: State, X: Family + Ord + Clone> SearchContext<F, Q, X> {
	fn new(depth: usize) -> SearchContext<F, Q, X> {
		let data = SearchContextData {
			depth,
			parent: None,
			atoms: Vec::new(),
			bindings: HashMap::new(),
			sub_problems: OnceCell::new()
		};
		SearchContext::from(data)
	}

	fn from(data: SearchContextData<F, Q, X>) -> SearchContext<F, Q, X> {
		SearchContext {
			p: Rc::new(data)
		}
	}
}

impl<F: Symbol, Q: State, X: Family + Ord + Clone> SearchContextData<F, Q, X> {
	fn from(depth: usize, parent: &SearchContext<F, Q, X>) -> SearchContextData<F, Q, X> {
		SearchContextData {
			depth,
			parent: Some(parent.p.clone()),
			atoms: Vec::new(),
			bindings: HashMap::new(),
			sub_problems: OnceCell::new()
		}
	}

	fn instance(&self, x: &X) -> Option<&(F, Vec<X>)> {
		if let Some(instance) = self.bindings.get(x) {
			Some(instance)
		} else {
			if let Some(parent) = &self.parent {
				parent.instance(x)
			} else {
				None
			}
		}
	}

	fn instanciate(&mut self, x: &X, _q: &Q, f: &F, arity: usize) -> &(F, Vec<X>) {
		let vars = (0..arity).map(|_| {
			let y = x.generate();
			// println!("{} > {}", x, y);
			y
		}).collect();
		self.bindings.insert(x.clone(), (f.clone(), vars));
		self.bindings.get(x).as_ref().unwrap()
	}

	fn looping_sub_problems(&self, sub_problems: &[SubProblem<F, X, Q>]) -> bool {
		if let Some(sps) = self.sub_problems.get() {
			// println!("looping in");
			let r1 = sub_problems.iter().any(|sub_problem| sps.iter().any(|sp| sub_problem.is_descendent_matching(sp)));
			// println!("looping mid");
			// let r2 = sps.iter().all(|sp| sub_problems.iter().any(|sub_problem| sub_problem.matches(sp)));

			if r1 { // TODO optimize
				// println!("loop detected");
				return true
			}
			// println!("OUT");
		}

		if let Some(parent) = self.parent.as_ref() {
			parent.looping_sub_problems(sub_problems)
		} else {
			false
		}
	}

	fn depth_bindings(&self, map: &mut HashMap<X, Pattern<F, X>>) {
		if let Some(parent) = &self.parent {
			if self.depth == parent.depth {
				parent.depth_bindings(map)
			}
		}

		for (x, (f, subs)) in &self.bindings {
			let xp = Pattern::cons(f.clone(), subs.iter().map(|x| Pattern::var(x.clone())).collect());

			for (_y, yp) in map.iter_mut() {
				*yp = yp.map_variables(&|z| {
					if z == x {
						xp.clone()
					} else {
						Pattern::var(z.clone())
					}
				})
			}

			map.insert(x.clone(), xp);
		}
	}

	fn depth_atoms(&self) -> Atoms<F, X, Q> {
		let mut map = HashMap::new();
		self.depth_bindings(&mut map);
		Atoms {
			context: Some(self),
			context_atoms: self.atoms.iter(),
			map
		}
	}
}

impl<F: Symbol + Ranked, Q: State, X: Family + Ord + Clone> SearchPattern<Rank<Convoluted<F>>, Q, SearchContext<F, Q, X>> for Convoluted<Pattern<F, X>> {
	fn matches(&self, depth: usize, ctx: &SearchContext<F, Q, X>, current_state: &Q, current_conf: &Configuration<Rank<Convoluted<F>>, Q>) -> Option<(SearchContext<F, Q, X>, Vec<Convoluted<Pattern<F, X>>>)> {
		let conf_f = current_conf.symbol();
		// let sub_states = current_conf.states();

		// println!("visiting {}({}) -> {}", conf_f, PList(sub_states), current_state);

		// let mut new_instances = HashMap::new();
		let mut new_context = SearchContextData::from(depth, ctx);
		let mut patterns_builder = ConvolutedPatternBuilder::new();
		// patterns_builder.set_arity(sub_states.len());

		for (k, pattern) in self.0.iter().enumerate() {
			if let MaybeBottom::Some(pattern) = pattern {
				match pattern.kind() {
					PatternKind::Cons(f, sub_patterns) => {
						if conf_f.0[k] != *f {
							// println!("rejected because of symbol inequality ({} != {})", conf_f.0[k], f);
							return None
						}

						for (i, sub_pattern) in sub_patterns.iter().enumerate() {
							patterns_builder.set(k, i, sub_pattern.clone())
						}
					},
					PatternKind::Var(x) => {
						// Is the variable already instanciated ?
						// if let Some((f, sub_variables)) = ctx.instance(x) {
						// 	if conf_f.0[k] != *f {
						// 		// println!("rejected because of symbol inequality ({} != {})", conf_f.0[k], f);
						// 		return None
						// 	}
						//
						// 	for (i, y) in sub_variables.iter().enumerate() {
						// 		patterns_builder.set(k, i, Pattern::var(y.clone()))
						// 	}
						// } else {
							// Have we instanciated it in a previous loop iteration (in the new context) ?
							if let Some((f, sub_variables)) = new_context.instance(x) {
								if conf_f.0[k] != *f {
									// println!("rejected because of symbol inequality ({} != {})", conf_f.0[k], f);
									return None
								}

								for (i, y) in sub_variables.iter().enumerate() {
									patterns_builder.set(k, i, Pattern::var(y.clone()))
								}
							} else {
								// Can we instanciate the variable ?
								// We cannot instanciate a variable with bottom.
								if let MaybeBottom::Some(f) = &conf_f.0[k] {
									// println!("instanciating {} with {}", x, Configuration(conf_f.clone(), sub_states.clone()));
									let (_, sub_variables) = new_context.instanciate(x, current_state, f, f.arity());

									for (i, y) in sub_variables.iter().enumerate() {
										patterns_builder.set(k, i, Pattern::var(y.clone()))
									}
								} else {
									// println!("rejected because symbol is not some");
									// println!("{}", self);
									return None
								}
							}
						// }
					}
				}
			} else {
				if conf_f.0[k] != MaybeBottom::Bottom {
					// println!("rejected because symbol is not bottom (but {})", conf_f.0[k]);
					return None
				}
			}
		}

		let patterns = patterns_builder.unwrap();
		new_context.atoms.push(Atom(self.clone(), current_state.clone()));

		// println!("size: {}/{}", patterns.len(), sub_states.len());

		let new_context = SearchContext::from(new_context);
		// if ta::bottom_up::SearchContext::looping(&new_context) {
		// 	None
		// } else {
			Some((new_context, patterns))
		// }
	}
}

pub fn search<'a, F: Symbol + Ranked, Q: State, X: Family + Ord + Clone>(aut: &'a Automaton<Rank<Convoluted<F>>, Q, NoLabel>, patterns: Vec<Convoluted<Pattern<F, X>>>, kill_signal: Option<crossbeam_channel::Receiver<()>>) -> Mux<TermFragment<'a, Automaton<Rank<Convoluted<F>>, Q, NoLabel>, Rank<Convoluted<F>>, Q, SearchContext<F, Q, X>, Convoluted<Pattern<F, X>>>> {
	// let ctx_patterns: Vec<Convoluted<Pattern<F, X>>> = patterns.iter().map(|ap| {
	// 	Convoluted(ap.0.iter().map(|p| {
	// 		match p {
	// 			MaybeBottom::Bottom => MaybeBottom::Bottom,
	// 			MaybeBottom::Some(p) => {
	// 				MaybeBottom::Some(p.map_variables(&|x| {
	// 					Pattern::var(Variable::from(x.clone()))
	// 				}))
	// 			}
	// 		}
	// 	}).collect())
	// }).collect();

	// let leaf = aut.final_states().next().unwrap().clone();

	let mut iterators = Vec::new();
	for leaves in combinations(&patterns, |_| aut.final_states().map(|q| (*q).clone())) {
		// println!("leaves: {:?}", leaves);
		iterators.push(TermFragment::new(aut, 0, SearchContext::new(0), leaves, patterns.clone(), kill_signal.clone()))
	}

	Mux::new(iterators)
}

pub fn multi_search<'a, 'e, F: Symbol + Ranked, Q: State, X: Family + Ord + Clone>(automata: &'a [&'e Automaton<Rank<Convoluted<F>>, Q, NoLabel>], patterns: Vec<Convoluted<Pattern<F, X>>>, kill_signal: Option<crossbeam_channel::Receiver<()>>) -> Mux<TermFragment<'a, [&'e Automaton<Rank<Convoluted<F>>, Q, NoLabel>], Rank<Convoluted<F>>, Indexed<Q>, SearchContext<F, Indexed<Q>, X>, Convoluted<Pattern<F, X>>>> {
	// let ctx_patterns: Vec<Convoluted<Pattern<F, X>>> = patterns.iter().map(|ap| {
	// 	Convoluted(ap.0.iter().map(|p| {
	// 		match p {
	// 			MaybeBottom::Bottom => MaybeBottom::Bottom,
	// 			MaybeBottom::Some(p) => {
	// 				MaybeBottom::Some(p.map_variables(&|x| {
	// 					Pattern::var(Variable::from(x.clone()))
	// 				}))
	// 			}
	// 		}
	// 	}).collect())
	// }).collect();

	// let leaf = aut.final_states().next().unwrap().clone();

	let mut iterators = Vec::new();
	let indexes: Vec<usize> = (0..patterns.len()).collect();
	for leaves in combinations(&indexes, |i| automata[*i].final_states().map(move |q| Indexed((*q).clone(), *i))) {
		// println!("leaves: {:?}", leaves);
		iterators.push(TermFragment::new(automata, 0, SearchContext::new(0), leaves, patterns.clone(), kill_signal.clone()))
	}

	Mux::new(iterators)
}
