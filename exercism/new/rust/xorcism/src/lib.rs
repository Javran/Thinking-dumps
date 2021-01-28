/// A munger which XORs a key with some data
#[derive(Clone)]
pub struct Xorcism<'a> {
    key : &'a[u8],
    pos: usize,
}

impl<'a> Xorcism<'a> {
    // Note: I don't know why new needs to be parametrized over <Key> while
    // not giving any constraint or instructions about what Key is supposed to be.
    // so instead I changed that to just [u8]

    /// Create a new Xorcism munger from a key
    ///
    /// Should accept anything which has a cheap conversion to a byte slice.
    pub fn new<Key: IntoIterator<Item=u8>>(key: &Key) -> Xorcism<'a> {
        let xs = key.into_iter().collect::<Vec<u8>>().as_slice();
        // Xorcism{key: ), pos: 0}
        unimplemented!();
    }

    fn next(&mut self) -> u8 {
        let result = self.key[self.pos];
        self.pos += 1;
        if self.pos == self.key.len() {
            self.pos = 0
        }
        result
    }

    /// XOR each byte of the input buffer with a byte from the key.
    ///
    /// Note that this is stateful: repeated calls are likely to produce different results,
    /// even with identical inputs.
    pub fn munge_in_place(&mut self, data: &mut [u8]) {
        for i in 0..data.len() {
            data[i] ^= self.next()
        }
    }

    /// XOR each byte of the data with a byte from the key.
    ///
    /// Note that this is stateful: repeated calls are likely to produce different results,
    /// even with identical inputs.
    ///
    /// Should accept anything which has a cheap conversion to a byte iterator.
    /// Shouldn't matter whether the byte iterator's values are owned or borrowed.
    pub fn munge<Data>(&mut self, data: Data) -> impl Iterator<Item = u8> {
        unimplemented!();
        // this empty iterator silences a compiler complaint that
        // () doesn't implement ExactSizeIterator
        std::iter::empty()
    }
}
