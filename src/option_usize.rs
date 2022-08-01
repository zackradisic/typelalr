#[derive(Clone, Copy, Debug)]
pub struct OptionUsize<T: Copy + Into<usize> + From<usize> = usize>(T);

impl<T: Copy + Into<usize> + From<usize>> OptionUsize<T> {
    #[inline]
    pub fn unwrap(self) -> T {
        if self.0.into() == usize::MAX {
            panic!("OptionUsize is none")
        }
        self.0
    }

    #[inline]
    pub fn unwrap_or(self, or: T) -> T {
        if self.0.into() == usize::MAX {
            or
        } else {
            self.0
        }
    }

    #[inline]
    pub fn some(val: T) -> Self {
        OptionUsize(val)
    }

    #[inline]
    pub fn none() -> Self {
        OptionUsize(T::from(usize::MAX))
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        self.0.into() == usize::MAX
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        self.0.into() != usize::MAX
    }

    pub fn map<A, F: Fn(usize) -> Option<A>>(&self, f: F) -> Option<A> {
        if self.0.into() == usize::MAX {
            None
        } else {
            f(self.0.into())
        }
    }
}

impl From<usize> for OptionUsize {
    fn from(val: usize) -> Self {
        Self::some(val)
    }
}
