use ta::{
    Symbol,
    Rank,
    NoLabel,
    bottom_up::{
        Automaton,
        Configuration,
        width_search::LanguageState
    },
    combinations
};
use crate::{
    Convoluted,
    MaybeBottom,
};

pub fn add_to_convolution<E, F: Symbol, Q: LanguageState<F, E>>(aut: &mut Automaton<Rank<Convoluted<F>>, Convoluted<Q>, NoLabel>, state: &Convoluted<Q>, env: &E) {
    if !aut.includes(state) {
        for configurations in combinations(&state.0, |q| {
            if let MaybeBottom::Some(q) = q {
                Box::new(q.configurations(env).map(|conf| MaybeBottom::Some(conf))) as Box<dyn Iterator<Item=MaybeBottom<Configuration<F, Q>>>>
            } else {
                Box::new(std::iter::once(MaybeBottom::Bottom)) as Box<dyn Iterator<Item=MaybeBottom<Configuration<F, Q>>>>
            }
        }) {
            let mut convoluted_f = Vec::with_capacity(configurations.len());
            let mut bottom = true;
            let mut states = Vec::new();

            for conf in &configurations {
                if let MaybeBottom::Some(Configuration(f, subs)) = conf {
                    bottom = false;

                    if states.len() < subs.len() {
                        let mut bottom_vec = Vec::new();
                        bottom_vec.resize(convoluted_f.len(), MaybeBottom::Bottom);
                        states.resize(subs.len(), bottom_vec);
                    }

                    convoluted_f.push(MaybeBottom::Some(f.clone()));

                    for (i, sub) in subs.iter().enumerate() {
                        states[i].push(MaybeBottom::Some(sub.clone()))
                    }

                    for i in subs.len()..states.len() {
                        states[i].push(MaybeBottom::Bottom);
                    }
                } else {
                    convoluted_f.push(MaybeBottom::Bottom);

                    for i in 0..states.len() {
                        states[i].push(MaybeBottom::Bottom);
                    }
                }
            }

            if !bottom {
                let subs: Vec<Convoluted<Q>> = states.into_iter().map(|states| Convoluted(states)).collect();
                let conf = Configuration(Rank(Convoluted(convoluted_f), subs.len()), subs.clone());
                aut.add(conf, NoLabel, state.clone());

                for sub in &subs {
                    add_to_convolution(aut, &sub, env);
                }
            }
        }
    }
}

pub fn state_convolution<E, F: Symbol, Q: LanguageState<F, E>>(initial_state: Convoluted<Q>, env: &E) -> Automaton<Rank<Convoluted<F>>, Convoluted<Q>, NoLabel> {
    let mut aut = Automaton::new();
    //let initial_state = Convoluted(states.iter().map(|q| MaybeBottom::Some(q.clone())).collect());
    add_to_convolution(&mut aut, &initial_state, env);
    aut.set_final(initial_state);
    aut
}
