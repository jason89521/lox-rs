#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        Self {
            start: value.0,
            end: value.1,
        }
    }
}

pub trait GetSpan {
    fn span(&self) -> Span;
}
