let List/map = https://prelude.dhall-lang.org/v17.1.0/List/map

let ID = Text

let CompassPoint = < _ | c | e | n | ne | nw | s | se | sw | w >

let Port =
      < ID : ID
      | CompassPoint : CompassPoint
      | Both : { id : ID, compassPoint : CompassPoint }
      >

let Attribute = { key : ID, value : ID }

let AttributeType = < edge | graph | node >

let NodeID = { id : ID, port : Optional Port }

let Statement =
      ∀(Statement : Type) →
      ∀(Vertex : Type) →
      ∀(SubGraph : Type) →
      ∀ ( constructors
        : { statement :
              { node : { id : ID, port : Optional Port } → Statement
              , edges :
                  { vertices : List Vertex, attributes : List Attribute } →
                    Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    Statement
              , attribute : Attribute → Statement
              , subgraph : SubGraph → Statement
              }
          , vertex : { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
          , subgraph :
              { id : Optional Text, statements : List Statement } → SubGraph
          }
        ) →
        Statement

let Vertex =
      ∀(Statement : Type) →
      ∀(Vertex : Type) →
      ∀(SubGraph : Type) →
      ∀ ( constructors
        : { statement :
              { node : { id : ID, port : Optional Port } → Statement
              , edges :
                  { vertices : List Vertex, attributes : List Attribute } →
                    Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    Statement
              , attribute : Attribute → Statement
              , subgraph : SubGraph → Statement
              }
          , vertex : { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
          , subgraph :
              { id : Optional Text, statements : List Statement } → SubGraph
          }
        ) →
        Vertex

let SubGraph =
      ∀(Statement : Type) →
      ∀(Vertex : Type) →
      ∀(SubGraph : Type) →
      ∀ ( constructors
        : { statement :
              { node : { id : ID, port : Optional Port } → Statement
              , edges :
                  { vertices : List Vertex, attributes : List Attribute } →
                    Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    Statement
              , attribute : Attribute → Statement
              , subgraph : SubGraph → Statement
              }
          , vertex : { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
          , subgraph :
              { id : Optional Text, statements : List Statement } → SubGraph
          }
        ) →
        SubGraph

let statement
    : { node : { id : ID, port : Optional Port } → Statement
      , edges :
          { vertices : List Vertex, attributes : List Attribute } → Statement
      , attributes :
          { type : AttributeType, attributes : List Attribute } → Statement
      , attribute : Attribute → Statement
      , subgraph : SubGraph → Statement
      }
    = { node =
          λ(x : { id : ID, port : Optional Port }) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.statement.node x
      , edges =
          λ(x : { vertices : List Vertex, attributes : List Attribute }) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.statement.edges
              { vertices =
                  let adapt =
                        λ(v : Vertex@1) →
                          v Statement Vertex SubGraph constructors

                  in  List/map Vertex@1 Vertex adapt x.vertices
              , attributes = x.attributes
              }
      , attributes =
          λ(x : { type : AttributeType, attributes : List Attribute }) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.statement.attributes x
      , attribute =
          λ(x : Attribute) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.statement.attribute x
      , subgraph =
          λ(x : SubGraph) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.statement.subgraph
              (x Statement Vertex SubGraph constructors)
      }

let vertex
    : { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
    = { nodeID =
          λ(x : { id : ID, port : Optional Port }) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.vertex.nodeID x
      , subgraph =
          λ(x : SubGraph) →
          λ(Statement : Type) →
          λ(Vertex : Type) →
          λ(SubGraph : Type) →
          λ ( constructors
            : { statement :
                  { node : { id : ID, port : Optional Port } → Statement
                  , edges :
                      { vertices : List Vertex, attributes : List Attribute } →
                        Statement
                  , attributes :
                      { type : AttributeType, attributes : List Attribute } →
                        Statement
                  , attribute : Attribute → Statement
                  , subgraph : SubGraph → Statement
                  }
              , vertex :
                  { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
              , subgraph :
                  { id : Optional Text, statements : List Statement } → SubGraph
              }
            ) →
            constructors.vertex.subgraph
              (x Statement Vertex SubGraph constructors)
      }

let subgraph
    : { id : Optional ID, statements : List Statement } → SubGraph
    = λ(x : { id : Optional ID, statements : List Statement }) →
      λ(Statement : Type) →
      λ(Vertex : Type) →
      λ(SubGraph : Type) →
      λ ( constructors
        : { statement :
              { node : { id : ID, port : Optional Port } → Statement
              , edges :
                  { vertices : List Vertex, attributes : List Attribute } →
                    Statement
              , attributes :
                  { type : AttributeType, attributes : List Attribute } →
                    Statement
              , attribute : Attribute → Statement
              , subgraph : SubGraph → Statement
              }
          , vertex : { nodeID : NodeID → Vertex, subgraph : SubGraph → Vertex }
          , subgraph :
              { id : Optional Text, statements : List Statement } → SubGraph
          }
        ) →
        constructors.subgraph
          { id = x.id
          , statements =
              let adapt =
                    λ(s : Statement@1) →
                      s Statement Vertex SubGraph constructors

              in  List/map Statement@1 Statement adapt x.statements
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
