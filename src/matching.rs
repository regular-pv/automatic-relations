use std::marker::PhantomData;

pub struct PatternMatcher<F> {
    f: PhantomData<F>
}
