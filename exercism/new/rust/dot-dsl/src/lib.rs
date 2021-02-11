
pub mod graph {
    use std::collections::HashSet;

    // What is the purpose of having this unnecessarily nested module structure???
    use graph_items::node::Node;
    use graph_items::edge::Edge;

    pub struct Graph<'a> {
        pub nodes: HashSet<Node<'a>>,
        pub edges: Vec<Edge>,
        pub attrs: Vec<u32>,
    }

    impl <'a> Graph<'a> {
        pub fn new() -> Self {
            unimplemented!("Construct a new Graph struct.");
        }
    }

    pub mod graph_items {
        pub mod edge {
            pub struct Edge;
        }

        pub mod node {
            pub struct Node<'a> {
                pub name: &'a str,
            }

            impl <'a> Node <'a> {
                pub fn new(name: &'a str) -> Self {
                    Node{name}
                }
            }
        }
    }
}
