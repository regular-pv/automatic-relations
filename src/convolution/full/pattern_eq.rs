use std::marker::PhantomData;
use std::hash::Hash;
use std::fmt;
use terms::{Pattern, PatternKind};
use ta::{combinations, combinations_option, combinations_weak_option, Configuration, ConfigurationIterator, SortedWith, LanguageState};
use crate::Convoluted;

pub trait Symbol = Clone + Hash + Eq + fmt::Debug;
pub trait Variable = Ord + Clone + Hash + Eq + fmt::Debug;
pub trait State = ta::State + fmt::Debug;

pub struct PatternEq<F, Q, X: SortedWith<Q>> {
    inner: Option<Pattern<F, MaybeBound<Q, X>>>,
    q: PhantomData<Q>
}

impl<F, Q, X: SortedWith<Q>> PatternEq<F, Q, X> {
    pub fn new(pattern: Option<Pattern<F, MaybeBound<Q, X>>>) -> PatternEq<F, Q, X> {
        PatternEq {
            inner: pattern,
            q: PhantomData
        }
    }
}

impl<'a, F: Clone, Q, X: Clone + SortedWith<Q>> From<&'a Pattern<F, X>> for PatternEq<F, Q, X> {
    fn from(pattern: &'a Pattern<F, X>) -> PatternEq<F, Q, X> {
        PatternEq::new(Some(pattern.map_variables(&|x| Pattern::var(MaybeBound::Bound(x.clone())))))
    }
}

impl<F: Clone, Q, X: Clone + SortedWith<Q>> From<Pattern<F, X>> for PatternEq<F, Q, X> {
    fn from(pattern: Pattern<F, X>) -> PatternEq<F, Q, X> {
        PatternEq::new(Some(pattern.map_variables(&|x| Pattern::var(MaybeBound::Bound(x.clone())))))
    }
}

impl<F: Clone, Q: Clone, X: Clone + SortedWith<Q>> Clone for PatternEq<F, Q, X> {
    fn clone(&self) -> PatternEq<F, Q, X> {
        PatternEq {
            inner: self.inner.clone(),
            q: PhantomData
        }
    }
}

impl<F: PartialEq, Q: PartialEq, X: PartialEq + SortedWith<Q>> PartialEq for PatternEq<F, Q, X> {
    fn eq(&self, other: &PatternEq<F, Q, X>) -> bool {
        self.inner == other.inner
    }
}

impl<F: Eq, Q: Eq, X: Eq + SortedWith<Q>> Eq for PatternEq<F, Q, X> {}

impl<F: Hash, Q: Hash, X: Hash + SortedWith<Q>> Hash for PatternEq<F, Q, X> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<F: fmt::Debug, Q: fmt::Debug, X: SortedWith<Q> + fmt::Debug> fmt::Debug for PatternEq<F, Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.inner {
            Some(inner) => write!(f, "[{:?}]", inner),
            None => write!(f, "[⟘]")
        }
    }
}

impl<F: fmt::Display, Q: fmt::Display, X: SortedWith<Q> + fmt::Display> fmt::Display for PatternEq<F, Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.inner {
            Some(inner) => write!(f, "[{}]", inner),
            None => write!(f, "[⟘]")
        }
    }
}

impl<F: Symbol, G, Q: State + LanguageState<F, G>, X: Variable + SortedWith<Q>> LanguageState<Convoluted<F>, (G, [X])> for PatternEq<F, Q, X> {
    //type Configurations = PatternEqConfigurations<'a, F, G, Q, X>;

    fn configurations<'a>(&'a self, lang: &'a (G, [X])) -> Box<ConfigurationIterator<'a, Convoluted<F>, Self> + 'a> {
        Box::new(PatternEqConfigurations::new(self, &lang.1, &lang.0))
    }
}

#[derive(Debug)]
pub enum MaybeBound<Q, X: SortedWith<Q>> {
    Bound(X),
    Unbound(Q)
}

impl<Q: fmt::Display, X: SortedWith<Q> + fmt::Display> fmt::Display for MaybeBound<Q, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MaybeBound::Bound(x) => x.fmt(f),
            MaybeBound::Unbound(q) => write!(f, "_:{}", q)
        }
    }
}

impl<Q: Clone, X: Clone + SortedWith<Q>> Clone for MaybeBound<Q, X> {
    fn clone(&self) -> MaybeBound<Q, X> {
        match self {
            MaybeBound::Bound(x) => MaybeBound::Bound(x.clone()),
            MaybeBound::Unbound(q) => MaybeBound::Unbound(q.clone())
        }
    }
}

impl<Q: Hash, X: Hash + SortedWith<Q>> Hash for MaybeBound<Q, X> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            MaybeBound::Bound(x) => x.hash(state),
            MaybeBound::Unbound(q) => q.hash(state)
        }
    }
}

impl<Q: PartialEq, X: PartialEq + SortedWith<Q>> PartialEq for MaybeBound<Q, X> {
    fn eq(&self, other: &MaybeBound<Q, X>) -> bool {
        match (self, other) {
            (MaybeBound::Bound(x), MaybeBound::Bound(y)) => x == y,
            (MaybeBound::Unbound(qx), MaybeBound::Unbound(qy)) => qx == qy,
            _ => false
        }
    }
}

impl<Q: Eq, X: Eq + SortedWith<Q>> Eq for MaybeBound<Q, X> {}

pub struct PatternEqConfigurations<'a, F: 'a + Symbol, G: 'a, Q: 'a + State + LanguageState<F, G>, X: 'a + Variable + SortedWith<Q>> {
    pattern: &'a PatternEq<F, Q, X>,
    variables: &'a [X],
    lang: &'a G,
    state: PatternEqConfigurationsState<'a, F, Q, X>
}

pub enum PatternEqConfigurationsState<'a, F: 'a + Symbol, Q: 'a + State, X: 'a + Variable + SortedWith<Q>> {
    Pattern {
        f: &'a F,
        sub_patterns: &'a Vec<Pattern<F, MaybeBound<Q, X>>>,
        configurations: Box<dyn Iterator<Item=Vec<Option<Configuration<F, Q>>>> + 'a>
    },
    Var {
        x: &'a X,
        k: usize,
        configurations: Box<dyn Iterator<Item=Vec<Option<Configuration<F, Q>>>> + 'a>
    },
    Any {
        sort: &'a Q,
        it: Box<dyn ta::ConfigurationIterator<'a, F, Q, Item = ta::Configuration<F, Q>> + 'a>,
        current_configurations: Option<Vec<Option<Configuration<F, Q>>>>,
        configurations: Box<dyn Iterator<Item=Vec<Option<Configuration<F, Q>>>> + 'a>
    },
    Bottom {
        configurations: Box<dyn Iterator<Item=Vec<Option<Configuration<F, Q>>>> + 'a>
    }
}
//
impl<'a, F: 'a + Symbol, G: 'a, Q: 'a + State + LanguageState<F, G>, X: 'a + Variable + SortedWith<Q>> PatternEqConfigurations<'a, F, G, Q, X> {
    pub fn new(pattern: &'a PatternEq<F, Q, X>, variables: &'a [X], lang: &'a G) -> PatternEqConfigurations<'a, F, G, Q, X> {
        let state = match &pattern.inner {
            Some(inner) => {
                match inner.kind() {
                    PatternKind::Cons(f, sub_patterns) => {
                        PatternEqConfigurationsState::Pattern {
                            f: f,
                            sub_patterns: sub_patterns.as_ref(),
                            configurations: Box::new(combinations_weak_option(variables, move |x| x.sort().configurations(lang)))
                        }
                    },
                    PatternKind::Var(MaybeBound::Bound(x)) => {
                        PatternEqConfigurationsState::Var {
                            x: x,
                            k: variables.binary_search(x).unwrap(),
                            configurations: Box::new(combinations_weak_option(variables, move |x| x.sort().configurations(lang)))
                        }
                    },
                    PatternKind::Var(MaybeBound::Unbound(sort)) => {
                        PatternEqConfigurationsState::Any {
                            sort: sort,
                            it: sort.configurations(lang),
                            current_configurations: None,
                            configurations: Box::new(combinations_weak_option(variables, move |x| x.sort().configurations(lang)))
                        }
                    }
                }
            },
            None => {
                PatternEqConfigurationsState::Bottom {
                    configurations: Box::new(combinations_weak_option(variables, move |x| x.sort().configurations(lang)))
                }
            }
        };

        PatternEqConfigurations {
            pattern: pattern,
            variables: variables,
            lang: lang,
            state: state
        }
    }
}

fn convoluted_symbol<F: Symbol, Q: State>(configurations: &Vec<Option<Configuration<F, Q>>>, k_conf: Option<&Configuration<F, Q>>) -> (Convoluted<F>, usize) {
    let mut arity;
    match k_conf {
        Some(conf) => {
            arity = conf.states().len();
        },
        None => arity = 0
    }
    for conf in configurations.iter() {
        match conf {
            Some(conf) => {
                if !conf.states().is_empty() {
                    if arity == 0 {
                        arity = conf.states().len();
                    } else {
                        arity *= conf.states().len();
                    }
                }
            },
            None => ()
        }
    }

    let mut convoluted_sym = Vec::with_capacity(arity);
    for conf in configurations.iter() {
        match conf {
            Some(conf) => {
                convoluted_sym.push(Some(conf.symbol().clone()));
            },
            None => {
                convoluted_sym.push(None)
            }
        }
    }
    match k_conf {
        Some(conf) => {
            convoluted_sym.push(Some(conf.symbol().clone()));
        },
        None => {
            convoluted_sym.push(None)
        }
    }
    (Convoluted(convoluted_sym), arity)
}

impl<'a, F: 'a + Symbol, G: 'a, Q: 'a + State + LanguageState<F, G>, X: 'a + Variable + SortedWith<Q>> Iterator for PatternEqConfigurations<'a, F, G, Q, X> {
    type Item = Configuration<Convoluted<F>, PatternEq<F, Q, X>>;

    fn next(&mut self) -> Option<Configuration<Convoluted<F>, PatternEq<F, Q, X>>> {
        let variables = self.variables;
        match self.state {
            PatternEqConfigurationsState::Pattern { f, sub_patterns, ref mut configurations } => {
                'outer_pattern: loop {
                    // we know `sub_patterns` is not empty,
                    // otherwise `pattern` would be a term.
                    match configurations.next() {
                        Some(configurations) => {
                            //println!("next conf");
                            // For each possible convoluted configuration: (f ⊗ configurations)(...),
                            // where `configurations` = (fx1 ⊗ ... ⊗ fxn) for each variable xi.
                            let mut arity = sub_patterns.len();
                            for conf in configurations.iter() {
                                match conf {
                                    Some(conf) => {
                                        if !conf.states().is_empty() {
                                            if arity == 0 {
                                                arity = conf.states().len();
                                            } else {
                                                arity *= conf.states().len();
                                            }
                                        }
                                    },
                                    None => ()
                                }
                            }

                            let mut convoluted_sym = Vec::with_capacity(arity);
                            for conf in configurations.iter() {
                                match conf {
                                    Some(conf) => {
                                        convoluted_sym.push(Some(conf.symbol().clone()));
                                    },
                                    None => {
                                        convoluted_sym.push(None)
                                    }
                                }
                            }
                            convoluted_sym.push(Some((*f).clone()));
                            let convoluted_sym = Convoluted(convoluted_sym);
                            // println!("convoluted {:?}:{}", convoluted_sym, arity);

                            //println!("convoluted");

                            let mut unfolded_states = Vec::new();

                            if arity > 0 {
                                let no_states = Vec::new();
                                for sub_states in combinations_option(&configurations, |conf| {
                                    match conf {
                                        Some(conf) => conf.states().iter().enumerate(),
                                        None => no_states.iter().enumerate()
                                    }
                                }) {
                                    // println!("sub_states: {:?}", sub_states);
                                    if sub_patterns.is_empty() {
                                        unfolded_states.push(PatternEq::new(None));
                                    } else {
                                        for sub_pattern in sub_patterns.iter() {
                                            if let Some(unfolded_pattern) = sub_pattern.try_map_variables(&|maybe_x| {
                                                match maybe_x {
                                                    MaybeBound::Bound(x) => {
                                                        let k = variables.binary_search(x).unwrap();
                                                        if let Some(conf) = &configurations[k] {
                                                            let mut unfolded_sub_patterns = Vec::with_capacity(conf.states().len());

                                                            match sub_states[k] {
                                                                Some((i, _)) => {
                                                                    for j in 0..conf.states().len() {
                                                                        if i == j {
                                                                            unfolded_sub_patterns.push(Pattern::var(MaybeBound::Bound(x.clone())))
                                                                        } else {
                                                                            unfolded_sub_patterns.push(Pattern::var(MaybeBound::Unbound(x.sort().clone())))
                                                                        }
                                                                    }
                                                                },
                                                                None => ()
                                                            }

                                                            Some(Pattern::cons(conf.symbol().clone(), unfolded_sub_patterns))
                                                        } else {
                                                            None
                                                        }
                                                    },
                                                    MaybeBound::Unbound(q) => {
                                                        Some(Pattern::var(MaybeBound::Unbound(q.clone())))
                                                    }
                                                }
                                            }) {
                                                unfolded_states.push(PatternEq::new(Some(unfolded_pattern)));
                                            } else {
                                                continue 'outer_pattern
                                            }
                                        }
                                    }
                                }
                            }

                            // this is the next one.
                            assert!(unfolded_states.len() == arity);
                            let convoluted_conf = Configuration(convoluted_sym, unfolded_states);
                            // println!("new conf {:?}", convoluted_conf);
                            return Some(convoluted_conf)
                        },
                        None => return None
                    }
                }
            },
            PatternEqConfigurationsState::Var { x, k, ref mut configurations } => {
                'outer_var: loop {
                    match configurations.next() {
                        Some(configurations) => {
                            let k_conf = &configurations[k];
                            match k_conf {
                                Some(k_conf) => {
                                    let (convoluted_sym, arity) = convoluted_symbol(&configurations, Some(&k_conf));

                                    let mut unfolded_states = Vec::new();

                                    let no_states = Vec::new();
                                    for sub_states in combinations_option(&configurations, |conf| {
                                        match conf {
                                            Some(conf) => conf.states().iter().enumerate(),
                                            None => no_states.iter().enumerate()
                                        }
                                    }) {
                                        if k_conf.states().is_empty() {
                                            unfolded_states.push(PatternEq::new(None))
                                        } else {
                                            let (i, _) = sub_states[k].unwrap(); // impossible to be None, or k_conf would be None too.
                                            for k_sub_state in k_conf.states().iter() {
                                                if i == k {
                                                    unfolded_states.push(PatternEq::new(Some(Pattern::var(MaybeBound::Bound(x.clone())))))
                                                } else {
                                                    unfolded_states.push(PatternEq::new(Some(Pattern::var(MaybeBound::Unbound(k_sub_state.clone())))))
                                                }
                                            }
                                        }
                                    }

                                    assert!(unfolded_states.len() == arity);
                                    let convoluted_conf = Configuration(convoluted_sym, unfolded_states);
                                    return Some(convoluted_conf)
                                },
                                None => continue 'outer_var
                            }
                        },
                        None => return None
                    }
                }
            },
            PatternEqConfigurationsState::Any { sort, ref mut it, ref mut current_configurations, ref mut configurations } => {
                loop {
                    match current_configurations {
                        Some(configurations) => {
                            match it.next() {
                                Some(conf) => {
                                    let (convoluted_sym, arity) = convoluted_symbol(&configurations, Some(&conf));
                                    // println!("convoluted {:?}:{}", convoluted_sym, arity);

                                    let mut unfolded_states = Vec::new();

                                    if arity > 0 {
                                        let no_states = Vec::new();
                                        for sub_states in combinations_option(&configurations, |conf| {
                                            match conf {
                                                Some(conf) => conf.states().iter().enumerate(),
                                                None => no_states.iter().enumerate()
                                            }
                                        }) {
                                            if conf.states().is_empty() {
                                                unfolded_states.push(PatternEq::new(None))
                                            } else {
                                                for k_sub_state in conf.states().iter() {
                                                    unfolded_states.push(PatternEq::new(Some(Pattern::var(MaybeBound::Unbound(k_sub_state.clone())))))
                                                }
                                            }
                                        }
                                    }

                                    assert!(unfolded_states.len() == arity);
                                    let convoluted_conf = Configuration(convoluted_sym, unfolded_states);
                                    return Some(convoluted_conf)
                                },
                                None => {
                                    *it = sort.configurations(self.lang);
                                    *current_configurations = None
                                }
                            }
                        },
                        None => {
                            match configurations.next() {
                                Some(configurations) => {
                                    *current_configurations = Some(configurations)
                                },
                                None => return None
                            }
                        }
                    }
                }
            },
            PatternEqConfigurationsState::Bottom { ref mut configurations } => {
                'outer_bottom: loop {
                    match configurations.next() {
                        Some(configurations) => {
                            if configurations.iter().find(|conf| conf.is_some()).is_none() {
                                continue 'outer_bottom
                            }

                            let (convoluted_sym, arity) = convoluted_symbol(&configurations, None);
                            // println!("convoluted {:?}:{}", convoluted_sym, arity);

                            let mut unfolded_states = Vec::new();

                            if arity > 0 {
                                let no_states = Vec::new();
                                for sub_states in combinations_option(&configurations, |conf| {
                                    match conf {
                                        Some(conf) => conf.states().iter(),
                                        None => no_states.iter()
                                    }
                                }) {
                                    unfolded_states.push(PatternEq::new(None))
                                }
                            }

                            assert!(unfolded_states.len() == arity);
                            let convoluted_conf = Configuration(convoluted_sym, unfolded_states);
                            return Some(convoluted_conf)
                        },
                        None => return None
                    }
                }
            }
        }
    }
}

impl<'a, F: 'a + Symbol, G: 'a, Q: 'a + State + LanguageState<F, G>, X: 'a + Variable + SortedWith<Q>> ConfigurationIterator<'a, Convoluted<F>, PatternEq<F, Q, X>> for PatternEqConfigurations<'a, F, G, Q, X> {
    // ...
}
