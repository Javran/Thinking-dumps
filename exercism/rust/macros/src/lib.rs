#[macro_export]
macro_rules! hashmap {
    () => {
        crate::HashMap::new()
    };
    (,) => {
        compile_error!("`hashmap!(,)` is not allowed.");
    };
    ($($k: expr => $v: expr),* $(,)?) => {
        {
            use crate::HashMap;
            let mut hm = HashMap::new();
            $(
                hm.insert($k, $v);
            )*
            hm
        }
    };
}
