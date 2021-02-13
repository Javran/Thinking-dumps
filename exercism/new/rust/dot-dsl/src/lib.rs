pub mod graph {
    use std::collections::HashMap;
    // What is the purpose of having this unnecessarily nested module structure???
    use graph_items::edge::Edge;
    use graph_items::node::Node;

    #[derive(Default)]
    pub struct Graph {
        pub nodes: Vec<Node>,
        pub edges: Vec<Edge>,
        pub attrs: HashMap<String, String>,
    }

    impl Graph {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn with_nodes(mut self, nodes: &[Node]) -> Self {
            self.nodes.append(&mut nodes.to_vec());
            self
        }

        pub fn with_edges(mut self, edges: &[Edge]) -> Self {
            self.edges.append(&mut edges.to_vec());
            self
        }

        pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
            self.attrs
                .extend(attrs.iter().map(|(k, v)| (k.to_string(), v.to_string())));
            self
        }

        pub fn get_node(self, node_name: &str) -> Option<Node> {
            self.nodes.iter().find(|n| n.name == node_name).cloned()
        }
    }

    pub mod graph_items {
        pub mod edge {
            use std::collections::HashMap;

            #[derive(PartialEq, Debug, Clone, Default)]
            pub struct Edge {
                pub src: String,
                pub dst: String,
                pub attrs: HashMap<String, String>,
            }

            impl Edge {
                pub fn new(src: &str, dst: &str) -> Self {
                    Edge {
                        src: src.to_string(),
                        dst: dst.to_string(),
                        attrs: HashMap::new(),
                    }
                }

                pub fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self {
                    self.attrs
                        .extend(attrs.iter().map(|(k, v)| (k.to_string(), v.to_string())));
                    self
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
                    self.attrs
                        .extend(attrs.iter().map(|(k, v)| (k.to_string(), v.to_string())));
                    self
                }

                pub fn get_attr(&self, key: &str) -> Option<&str> {
                    self.attrs.get(key).map(|v| v.as_str())
                }
            }
        }
    }
}
