pub mod graph {
    use std::collections::HashMap;
    // What is the purpose of having this unnecessarily nested module structure???
    use graph_items::edge::Edge;
    use graph_items::node::Node;

    pub struct Graph {
        pub nodes: Vec<Node>,
        pub edges: Vec<Edge>,
        pub attrs: HashMap<String, String>,
    }

    impl Graph {
        pub fn new() -> Self {
            Graph {
                nodes: vec![],
                edges: vec![],
                attrs: HashMap::new(),
            }
        }

        pub fn with_nodes(mut self, nodes: &Vec<Node>) -> Self {
            self.nodes = nodes.iter().cloned().collect();
            self
        }

        pub fn with_edges<T>(mut self, _edges: T) -> Self {
            unimplemented!()
        }

        pub fn with_attrs<T>(mut self, _attrs: T) -> Self {
            unimplemented!()
        }

        pub fn get_node<T>(self, _node: T) -> Option<Node> {
            unimplemented!()
        }
    }

    pub mod graph_items {
        pub mod edge {
            #[derive(PartialEq, Debug)]
            pub struct Edge {
                pub src: String,
                pub dst: String,
            }

            impl Edge {
                pub fn new(src: &str, dst: &str) -> Self {
                    Edge {
                        src: src.to_string(),
                        dst: dst.to_string(),
                    }
                }

                pub fn with_attrs<T>(&mut self, _attrs: T) -> Self {
                    unimplemented!()
                }
            }
        }

        pub mod node {
            use std::collections::HashMap;

            #[derive(PartialEq, Debug, Clone)]
            pub struct Node {
                pub name: String,
                pub attrs: HashMap<String, String>,
            }

            impl Node {
                pub fn new(name: &str) -> Self {
                    Node {
                        name: name.to_string(),
                        attrs: HashMap::new(),
                    }
                }

                pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
                    self.attrs = attrs
                        .iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect();
                    self
                }

                pub fn get_attr<T>(&self, _key: T) -> Option<&str> {
                    unimplemented!()
                }
            }
        }
    }
}
