use std::marker::PhantomData;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::cmp::{PartialOrd, Ordering};
use std::rc::Rc;
use terms::{
    Pattern,
    PatternKind,
    variable::Spawnable
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

pub struct Signature<F: Symbol, X: Spawnable, Q: State> {
    data: Vec<(Convoluted<Pattern<F, CtxVar<X>>>, Q)>
}

impl<F: Symbol + fmt::Display, X: Spawnable + fmt::Display, Q: State + fmt::Display> fmt::Display for Signature<F, X, Q> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<")?;
        for item in self.data.iter() {
            write!(f, "(")?;
            // for x in item.0.iter() {
            //     if let Some(x) = x {
            //         write!(f, "{}:", x)?;
            //     } else {
            //         write!(f, "_:")?;
            //     }
            // }
            write!(f, "{}:{}", item.0, item.1)?;
            write!(f, ")")?;
        }
        write!(f, ">")
    }
}

impl<F: Symbol + fmt::Display, X: Spawnable + fmt::Display, Q: State + fmt::Display> fmt::Debug for Signature<F, X, Q> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<F: Symbol, X: Spawnable, Q: State> Signature<F, X, Q> {
    pub fn new() -> Signature<F, X, Q> {
        Signature {
            data: Vec::new()
        }
    }

    pub fn union(&mut self, other: &Signature<F, X, Q>) {
        for s in other.data.iter() {
            self.data.push(s.clone());
        }
    }

    pub fn add(&mut self, patterns: &Convoluted<Pattern<F, CtxVar<X>>>, q: &Q) {
        self.data.push((patterns.clone(), q.clone()));
    }

    fn item_match(renaming: &HashMap<X, X>, other_item: &(Convoluted<Pattern<F, CtxVar<X>>>, Q), item: &(Convoluted<Pattern<F, CtxVar<X>>>, Q)) -> Option<HashMap<X, X>> {
        let mut new_renaming = renaming.clone();
        if item.1 == other_item.1 && (item.0).0.len() == (other_item.0).0.len() {
            for (i, y) in (other_item.0).0.iter().enumerate() {
                if let MaybeBottom::Some(y) = y {
                    if let MaybeBottom::Some(x) = &(item.0).0[i] {
                        if !x.renaming::<X, X, CtxVar<X>>(y, &mut new_renaming) {
                            return None
                        }
                    } else {
                        return None
                    }
                } else {
                    // we can have more constraints.
                }
            }
        } else {
            return None
        }

        Some(new_renaming)
    }

    /// Check if self is as strong as the given signature.
    /// This would mean that there exists a variable renaming for which each item of the other
    /// signature has an equivalent item in self (modulo renaming). And each variables in the
    /// renaming are parented.
    pub fn as_strong_as(&self, other: Option<&Signature<F, X, Q>>) -> bool {
        // println!("compare {:?} {:?}", self, other);

        if let Some(other) = other {
            let mut iterators = Vec::with_capacity(other.data.len());
            iterators.push(self.data.iter());
            //
            let mut current = Vec::with_capacity(other.data.len());
            let initial_renaming = HashMap::new();
            //
            while current.len() < other.data.len() {
                 let i = current.len();
                 let other_item = &other.data[i];

                 loop {
                    match iterators.last_mut().unwrap().next() {
                        Some(item) => {
                            let current_renaming = match current.first() {
                                Some(renaming) => renaming,
                                None => &initial_renaming
                            };
                            if let Some(new_renaming) = Self::item_match(current_renaming, other_item, item) {
                                current.push(new_renaming);
                                let j = i + 1;
                                if j < other.data.len() {
                                    iterators.push(self.data.iter());
                                }
                                break
                            }

                            // if let Some(new_renaming) = self.contains(
                            //     match current.first() { Some(renaming) => renaming, None => &initial_renaming },
                            //     item
                            // ) {
                            //     current.push(new_renaming);
                            //     let j = i + 1;
                            //     if j < other.data.len() {
                            //         iterators.push(other.data.iter());
                            //     }
                            //     break
                            // }
                        },
                        None => {
                            if i == 0 {
                                // println!("signature {} is NOT as strong as {}", self, other);
                                return false
                            }
                            iterators.pop();
                            current.pop();
                            break
                        }
                    }
                }
            }

            // println!("signature {} is as strong as {}", self, other);
            true
        } else {
            false
        }
    }
}

// impl<X: Spawnable, Q: State> PartialEq for Signature<X, Q> {
//     fn eq(&self, other: &Signature<X, Q>) -> bool {
//         false
//     }
// }

#[derive(Debug)]
pub struct Variable<X: Spawnable> {
    parent: Option<CtxVar<X>>,
    x: X,
}

impl<X: Spawnable + fmt::Display> fmt::Display for Variable<X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.x)
    }
}

impl<X: Spawnable> Variable<X> {
    pub fn new(parent: &CtxVar<X>) -> CtxVar<X> {
        CtxVar(Rc::new(Variable {
            parent: Some(parent.clone()),
            x: X::spawn(parent.as_ref().namespace())
        }))
    }

    pub fn from(x: X) -> CtxVar<X> {
        CtxVar(Rc::new(Variable {
            parent: None,
            x: x
        }))
    }

    pub fn signed_with<F: Symbol, Q: State>(this: &CtxVar<X>, sig: &Signature<F, X, Q>, ctx: &SearchContextData<F, Q, X>) -> bool {
        if sig.as_strong_as(ctx.signature_of(this)) {
            true
        } else {
            if let Some(parent) = this.parent() {
                Variable::signed_with(parent, sig, ctx)
            } else {
                false
            }
        }
    }
}

impl<X: Spawnable> AsRef<X> for Variable<X> {
    fn as_ref(&self) -> &X {
        &self.x
    }
}

impl<X: Spawnable> PartialEq for Variable<X> {
    fn eq(&self, other: &Variable<X>) -> bool {
        self.x == other.x
    }
}

impl<X: Spawnable> Eq for Variable<X> {
    //
}

impl<X: Spawnable> Hash for Variable<X> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CtxVar<X: Spawnable>(Rc<Variable<X>>);

impl<X: Spawnable + fmt::Display> fmt::Display for CtxVar<X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.x)
    }
}

impl<X: Spawnable> CtxVar<X> {
    pub fn parent(&self) -> &Option<CtxVar<X>> {
        &self.0.parent
    }

    pub fn child_of(&self, other: &CtxVar<X>) -> bool {
        if self.0.x == other.0.x {
            true
        } else {
            if let Some(parent) = self.parent() {
                parent.child_of(other)
            } else {
                false
            }
        }
    }
}

impl<X: Spawnable> AsRef<X> for CtxVar<X> {
    fn as_ref(&self) -> &X {
        &self.0.x
    }
}

impl<X: Spawnable> PartialOrd for CtxVar<X> {
    fn partial_cmp(&self, other: &CtxVar<X>) -> Option<Ordering> {
        if self.as_ref() == other.as_ref() {
            None // Some(Ordering::Equal)
        } else {
            if self.child_of(other) {
                Some(Ordering::Less)
            } else {
                if other.child_of(self) {
                    None // Some(Ordering::Greater)
                } else {
                    None
                }
            }
        }
    }
}

pub struct SearchContextData<F: Symbol, Q: State, X: Spawnable> {
    parent: Option<Rc<SearchContextData<F, Q, X>>>,
    table: HashMap<X, (F, Vec<CtxVar<X>>)>,
    signature: Signature<F, X, Q>,
    vars: HashSet<CtxVar<X>>,
    q: PhantomData<Q>
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

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Spawnable + fmt::Display> fmt::Display for SearchContextData<F, Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(parent) = &self.parent {
            write!(f, "{}", parent)?;
        }
        for (x, (sym, vars)) in self.table.iter() {
            write!(f, "{}: {}({}), ", x, sym, PList(vars))?;
        }

        Ok(())
    }
}

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Spawnable + fmt::Display> fmt::Debug for SearchContextData<F, Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub struct SearchContext<F: Symbol, Q: State, X: Spawnable> {
    p: Rc<SearchContextData<F, Q, X>>
}

impl<F: Symbol, Q: State, X: Spawnable> ta::bottom_up::width_search::SearchContext for SearchContext<F, Q, X> {
    //
}

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Spawnable + fmt::Display> fmt::Debug for SearchContext<F, Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.p, f)
    }
}

impl<F: Symbol + fmt::Display, Q: State + fmt::Display, X: Spawnable + fmt::Display> fmt::Display for SearchContext<F, Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.p, f)
    }
}

impl<F: Symbol, Q: State, X: Spawnable> SearchContext<F, Q, X> {
    fn new() -> SearchContext<F, Q, X> {
        let data = SearchContextData {
            parent: None,
            table: HashMap::new(),
            signature: Signature::new(),
            vars: HashSet::new(),
            q: PhantomData
        };
        SearchContext::from(data)
    }

    fn from(data: SearchContextData<F, Q, X>) -> SearchContext<F, Q, X> {
        SearchContext {
            p: Rc::new(data)
        }
    }

    fn instance(&self, x: &CtxVar<X>) -> Option<&(F, Vec<CtxVar<X>>)> {
        self.p.instance(x)
    }
}

impl<F: Symbol, Q: State, X: Spawnable> SearchContextData<F, Q, X> {
    fn from(parent: &SearchContext<F, Q, X>) -> SearchContextData<F, Q, X> {
        SearchContextData {
            parent: Some(parent.p.clone()),
            table: HashMap::new(),
            signature: Signature::new(),
            vars: HashSet::new(),
            q: PhantomData
        }
    }

    fn instance(&self, x: &CtxVar<X>) -> Option<&(F, Vec<CtxVar<X>>)> {
        if let Some(instance) = self.table.get(x.as_ref()) {
            Some(instance)
        } else {
            if let Some(parent) = &self.parent {
                parent.instance(x)
            } else {
                None
            }
        }
    }

    fn instanciate(&mut self, x: &CtxVar<X>, _q: &Q, f: &F, arity: usize) -> &(F, Vec<CtxVar<X>>) {
        let vars = (0..arity).map(|_| Variable::new(x)).collect();
        self.table.insert(x.as_ref().clone(), (f.clone(), vars));
        self.table.get(x.as_ref()).as_ref().unwrap()
    }

    fn signature_of(&self, x: &CtxVar<X>) -> Option<&Signature<F, X, Q>> {
        if self.vars.contains(x) {
            Some(&self.signature)
        } else {
            if let Some(parent) = &self.parent {
                parent.signature_of(x)
            } else {
                None
            }
        }
    }

    fn sign(&mut self, patterns: &Convoluted<Pattern<F, CtxVar<X>>>, q: &Q) -> bool {
        // new signature.
        // let mut vars = Vec::new();
        for pattern in patterns.0.iter() {
            if let MaybeBottom::Some(pattern) = pattern {
                'next_var: for x in pattern.variables() {
                    for pattern in patterns.0.iter() {
                        if let MaybeBottom::Some(pattern) = pattern {
                            for y in pattern.variables() {
                                if y < x {
                                    // its a parent.
                                    // we skip it.
                                    continue 'next_var
                                }
                            }
                        }
                    }

                    self.vars.insert(x.clone());

                    let sig_x = if let Some(parent) = &self.parent {
                        parent.signature_of(x)
                    } else {
                        None
                    };

                    if let Some(sig_x) = sig_x {
                        self.signature.union(sig_x)
                    }
                }

                // if let PatternKind::Var(x) = pattern.kind() {
                //     // check if it is not a parent
                //     for pattern in patterns.0.iter() {
                //         if let MaybeBottom::Some(pattern) = pattern {
                //             if let PatternKind::Var(y) = pattern.kind() {
                //                 if y < x {
                //                     // its a parent.
                //                     // we skip it.
                //                     continue 'next_pattern
                //                 }
                //             }
                //         }
                //     }
                //
                //     self.vars.insert(x.clone());
                //
                //     let sig_x = if let Some(parent) = &self.parent {
                //         parent.signature_of(x)
                //     } else {
                //         None
                //     };
                //
                //     if let Some(sig_x) = sig_x {
                //         self.signature.union(sig_x)
                //     }
                // } else {
                //     // vars.push(None)
                // }
            } else {
                // vars.push(None)
            }
        }

        self.signature.add(patterns, q);

        // println!("signature: {}", self.signature);

        // find this signature in the parents.
        if let Some(parent) = &self.parent {
            for x in self.vars.iter() {
                if Variable::signed_with(x, &self.signature, &parent) {
                    // println!("LOOP");
                    return false
                }
            }
        }

        true
    }
}

impl<F: Symbol + Ranked, Q: State, X: Spawnable> SearchPattern<Rank<Convoluted<F>>, Q, SearchContext<F, Q, X>> for Convoluted<Pattern<F, CtxVar<X>>> {
    fn matches(&self, ctx: &SearchContext<F, Q, X>, current_state: &Q, Configuration(conf_f, _): &Configuration<Rank<Convoluted<F>>, Q>) -> Option<(SearchContext<F, Q, X>, Vec<Convoluted<Pattern<F, CtxVar<X>>>>)> {
        // let mut new_instances = HashMap::new();
        let mut new_context = SearchContextData::from(ctx);
        if new_context.sign(self, current_state) { // return true only if no loop is detected.
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
                            if let Some((f, sub_variables)) = ctx.instance(x) {
                                if conf_f.0[k] != *f {
                                    // println!("rejected because of symbol inequality ({} != {})", conf_f.0[k], f);
                                    return None
                                }

                                for (i, y) in sub_variables.iter().enumerate() {
                                    patterns_builder.set(k, i, Pattern::var(y.clone()))
                                }
                            } else {
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
                            }
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
            // println!("size: {}/{}", patterns.len(), sub_states.len());
            Some((SearchContext::from(new_context), patterns))
        } else {
            // println!("rejected because of loop detection");
            None
        }
    }
}

pub fn search<'a, F: Symbol + Ranked, Q: State, X: Spawnable>(aut: &'a Automaton<Rank<Convoluted<F>>, Q, NoLabel>, patterns: Vec<Convoluted<Pattern<F, X>>>, kill_signal: crossbeam_channel::Receiver<()>) -> Mux<TermFragment<'a, Automaton<Rank<Convoluted<F>>, Q, NoLabel>, Rank<Convoluted<F>>, Q, SearchContext<F, Q, X>, Convoluted<Pattern<F, CtxVar<X>>>>> {
    let ctx_patterns: Vec<Convoluted<Pattern<F, CtxVar<X>>>> = patterns.iter().map(|ap| {
        Convoluted(ap.0.iter().map(|p| {
            match p {
                MaybeBottom::Bottom => MaybeBottom::Bottom,
                MaybeBottom::Some(p) => {
                    MaybeBottom::Some(p.map_variables(&|x| {
                        Pattern::var(Variable::from(x.clone()))
                    }))
                }
            }
        }).collect())
    }).collect();

    // let leaf = aut.final_states().next().unwrap().clone();

    let mut iterators = Vec::new();
    for leaves in combinations(&patterns, |_| aut.final_states().map(|q| (*q).clone())) {
        // println!("leaves: {:?}", leaves);
        iterators.push(TermFragment::new(aut, SearchContext::new(), leaves, ctx_patterns.clone(), kill_signal.clone()))
    }

    Mux::new(iterators)
}

pub fn multi_search<'a, 'e, F: Symbol + Ranked, Q: State, X: Spawnable>(automata: &'a [&'e Automaton<Rank<Convoluted<F>>, Q, NoLabel>], patterns: Vec<Convoluted<Pattern<F, X>>>, kill_signal: crossbeam_channel::Receiver<()>) -> Mux<TermFragment<'a, [&'e Automaton<Rank<Convoluted<F>>, Q, NoLabel>], Rank<Convoluted<F>>, Indexed<Q>, SearchContext<F, Indexed<Q>, X>, Convoluted<Pattern<F, CtxVar<X>>>>> {
    let ctx_patterns: Vec<Convoluted<Pattern<F, CtxVar<X>>>> = patterns.iter().map(|ap| {
        Convoluted(ap.0.iter().map(|p| {
            match p {
                MaybeBottom::Bottom => MaybeBottom::Bottom,
                MaybeBottom::Some(p) => {
                    MaybeBottom::Some(p.map_variables(&|x| {
                        Pattern::var(Variable::from(x.clone()))
                    }))
                }
            }
        }).collect())
    }).collect();

    // let leaf = aut.final_states().next().unwrap().clone();

    let mut iterators = Vec::new();
    let indexes: Vec<usize> = (0..patterns.len()).collect();
    for leaves in combinations(&indexes, |i| automata[*i].final_states().map(move |q| Indexed((*q).clone(), *i))) {
        // println!("leaves: {:?}", leaves);
        iterators.push(TermFragment::new(automata, SearchContext::new(), leaves, ctx_patterns.clone(), kill_signal.clone()))
    }

    Mux::new(iterators)
}
