let List/map =
      https://prelude.dhall-lang.org/v17.1.0/List/map.dhall sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let ID = Text

let CompassPoint = < _ | c | e | n | ne | nw | s | se | sw | w >

let Port =
      < ID : ID
      | CompassPoint : CompassPoint
      | Both : { id : ID, compassPoint : CompassPoint }
      >

let Attribute = { mapKey : ID, mapValue : ID }

let AttributeType = < edge | graph | node >

let NodeID = { id : ID, port : Optional Port }

let Statement =
      ∀(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
      ∀ ( constructors
        : { statement :
              { node :
                  { nodeID : NodeID, attributes : List Attribute } →
                    types.Statement
              , edges :
                  { vertices : List types.Vertex
                  , attributes : List Attribute
                  } →
                    types.Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    types.Statement
              , attribute : Attribute → types.Statement
              , subgraph : types.SubGraph → types.Statement
              }
          , vertex :
              { nodeID : NodeID → types.Vertex
              , subgraph : types.SubGraph → types.Vertex
              }
          , subgraph :
              { id : Optional Text
              , attributes : List Attribute
              , statements : List types.Statement
              } →
                types.SubGraph
          }
        ) →
        types.Statement

let Vertex =
      ∀(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
      ∀ ( constructors
        : { statement :
              { node :
                  { nodeID : NodeID, attributes : List Attribute } →
                    types.Statement
              , edges :
                  { vertices : List types.Vertex
                  , attributes : List Attribute
                  } →
                    types.Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    types.Statement
              , attribute : Attribute → types.Statement
              , subgraph : types.SubGraph → types.Statement
              }
          , vertex :
              { nodeID : NodeID → types.Vertex
              , subgraph : types.SubGraph → types.Vertex
              }
          , subgraph :
              { id : Optional Text
              , attributes : List Attribute
              , statements : List types.Statement
              } →
                types.SubGraph
          }
        ) →
        types.Vertex

let SubGraph =
      ∀(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
      ∀ ( constructors
        : { statement :
              { node :
                  { nodeID : NodeID, attributes : List Attribute } →
                    types.Statement
              , edges :
                  { vertices : List types.Vertex
                  , attributes : List Attribute
                  } →
                    types.Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    types.Statement
              , attribute : Attribute → types.Statement
              , subgraph : types.SubGraph → types.Statement
              }
          , vertex :
              { nodeID : NodeID → types.Vertex
              , subgraph : types.SubGraph → types.Vertex
              }
          , subgraph :
              { id : Optional Text
              , attributes : List Attribute
              , statements : List types.Statement
              } →
                types.SubGraph
          }
        ) →
        types.SubGraph

let statement
    : { node : { nodeID : NodeID, attributes : List Attribute } → Statement
      , edges :
          { vertices : List Vertex, attributes : List Attribute } → Statement
      , attributes :
          { type : AttributeType, attributes : List Attribute } → Statement
      , attribute : Attribute → Statement
      , subgraph : SubGraph → Statement
      }
    = { node =
          λ(x : { nodeID : NodeID, attributes : List Attribute }) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.statement.node x
      , edges =
          λ(x : { vertices : List Vertex, attributes : List Attribute }) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.statement.edges
              { vertices =
                  let adapt = λ(v : Vertex) → v types constructors

                  in  List/map Vertex types.Vertex adapt x.vertices
              , attributes = x.attributes
              }
      , attributes =
          λ(x : { type : AttributeType, attributes : List Attribute }) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.statement.attributes x
      , attribute =
          λ(x : Attribute) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.statement.attribute x
      , subgraph =
          λ(x : SubGraph) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.statement.subgraph (x types constructors)
      }

let vertex
    : { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
    = { nodeID =
          λ(x : { id : ID, port : Optional Port }) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.vertex.nodeID x
      , subgraph =
          λ(x : SubGraph) →
          λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
          λ ( constructors
            : { statement :
                  { node :
                      { nodeID : NodeID, attributes : List Attribute } →
                        types.Statement
                  , edges :
                      { vertices : List types.Vertex
                      , attributes : List Attribute
                      } →
                        types.Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        types.Statement
                  , attribute : Attribute → types.Statement
                  , subgraph : types.SubGraph → types.Statement
                  }
              , vertex :
                  { nodeID : NodeID → types.Vertex
                  , subgraph : types.SubGraph → types.Vertex
                  }
              , subgraph :
                  { id : Optional Text
                  , attributes : List Attribute
                  , statements : List types.Statement
                  } →
                    types.SubGraph
              }
            ) →
            constructors.vertex.subgraph (x types constructors)
      }

let subgraph
    : { id : Optional ID
      , attributes : List Attribute
      , statements : List Statement
      } →
        SubGraph
    = λ ( x
        : { id : Optional ID
          , attributes : List Attribute
          , statements : List Statement
          }
        ) →
      λ(types : { Statement : Type, Vertex : Type, SubGraph : Type }) →
      λ ( constructors
        : { statement :
              { node :
                  { nodeID : NodeID, attributes : List Attribute } →
                    types.Statement
              , edges :
                  { vertices : List types.Vertex
                  , attributes : List Attribute
                  } →
                    types.Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    types.Statement
              , attribute : Attribute → types.Statement
              , subgraph : types.SubGraph → types.Statement
              }
          , vertex :
              { nodeID : NodeID → types.Vertex
              , subgraph : types.SubGraph → types.Vertex
              }
          , subgraph :
              { id : Optional Text
              , attributes : List Attribute
              , statements : List types.Statement
              } →
                types.SubGraph
          }
        ) →
        constructors.subgraph
          { id = x.id
          , statements =
              let adapt = λ(s : Statement) → s types constructors

              in  List/map Statement types.Statement adapt x.statements
          , attributes = x.attributes
          }

let Directionality = < graph | digraph >

let Graph =
      { strict : Bool
      , directionality : Directionality
      , id : Optional ID
      , statements : List Statement
      }

in  { Attribute
    , AttributeType
    , CompassPoint
    , Directionality
    , Graph
    , ID
    , NodeID
    , Port
    , SubGraph
    , subgraph
    , Statement
    , statement
    , Vertex
    , vertex
    }
