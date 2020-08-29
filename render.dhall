let Text/concatSep = https://prelude.dhall-lang.org/v17.1.0/Text/concatSep.dhall

let Text/concatMapSep =
      https://prelude.dhall-lang.org/v17.1.0/Text/concatMapSep.dhall

let List/map = https://prelude.dhall-lang.org/v17.1.0/List/map.dhall

let List/null = https://prelude.dhall-lang.org/v17.1.0/List/null.dhall

let types = ./types.dhall

let compassPoint =
      λ(c : types.CompassPoint) →
        merge
          { n = "n"
          , ne = "ne"
          , e = "e"
          , se = "se"
          , s = "s"
          , sw = "sw"
          , w = "w"
          , nw = "nw"
          , c = "c"
          , _ = "_"
          }
          c

let id = Text/show

let directionality =
      λ(d : types.Directionality) →
        merge { graph = "graph", digraph = "digraph" } d

let port =
      λ(p : types.Port) →
        merge
          { CompassPoint = λ(c : types.CompassPoint) → ":${compassPoint c}"
          , ID = λ(i : types.ID) → ":${id i}"
          , Both =
              λ(b : { id : types.ID, compassPoint : types.CompassPoint }) →
                ":${id b.id}:${compassPoint b.compassPoint}"
          }
          p

let attribute = λ(a : types.Attribute) → "${id a.mapKey} = ${id a.mapValue}"

let attributeList =
      λ(`as` : List types.Attribute) →
        "[ ${Text/concatMapSep ", " types.Attribute attribute `as`} ]"

let attributeType =
      λ(t : types.AttributeType) →
        merge { graph = "graph", node = "node", edge = "edge" } t

let edge =
      λ(d : types.Directionality) → merge { graph = " -- ", digraph = " -> " } d

let nodeID =
      λ(n : types.NodeID) →
        let renderedPort = merge { None = "", Some = port } n.port

        in  "${id n.id}${renderedPort}"

let statement =
      λ(d : types.Directionality) →
      λ(s : types.Statement) →
        s
          { Statement = Text, Vertex = Text, SubGraph = Text }
          { statement =
            { node =
                λ ( x
                  : { nodeID : { id : types.ID, port : Optional types.Port }
                    , attributes : List types.Attribute
                    }
                  ) →
                  let renderedAttributeList =
                        if    List/null types.Attribute x.attributes
                        then  [] : List Text
                        else  [ attributeList x.attributes ]

                  in  Text/concatSep
                        " "
                        ([ nodeID x.nodeID ] # renderedAttributeList)
            , edges =
                λ ( x
                  : { vertices : List Text, attributes : List types.Attribute }
                  ) →
                  let renderedAttributeList =
                        if    List/null types.Attribute x.attributes
                        then  [] : List Text
                        else  [ attributeList x.attributes ]

                  in  Text/concatSep
                        " "
                        (   [ Text/concatSep (edge d) x.vertices ]
                          # renderedAttributeList
                        )
            , attributes =
                λ ( x
                  : { type : types.AttributeType
                    , attributes : List types.Attribute
                    }
                  ) →
                  "${attributeType x.type} ${attributeList x.attributes}"
            , attribute
            , subgraph = λ(x : Text) → x
            }
          , vertex = { nodeID, subgraph = λ(x : Text) → x }
          , subgraph =
              λ(x : { id : Optional Text, statements : List Text }) →
                let renderedID =
                      merge
                        { None = [] : List Text
                        , Some = λ(i : types.ID) → [ "subgraph", id i ]
                        }
                        x.id

                in  Text/concatSep
                      " "
                      (   renderedID
                        # [ "{" ]
                        # List/map
                            Text
                            Text
                            (λ(t : Text) → "${t};")
                            x.statements
                        # [ "}" ]
                      )
          }

let graph
    : types.Graph → Text
    = λ(g : types.Graph) →
        let renderedStrict = if g.strict then [ "strict" ] else [] : List Text

        let renderedID =
              merge
                { None = [] : List Text, Some = λ(i : types.ID) → [ id i ] }
                g.id

        let renderedStatements =
              List/map
                types.Statement
                Text
                (λ(s : types.Statement) → "${statement g.directionality s};")
                g.statements

        in  Text/concatSep
              " "
              (   renderedStrict
                # [ directionality g.directionality ]
                # renderedID
                # [ "{" ]
                # renderedStatements
                # [ "}" ]
              )

let minimalExample =
        assert
      :   graph
            { strict = False
            , directionality = types.Directionality.graph
            , id = None types.ID
            , statements = [] : List types.Statement
            }
        ≡ "graph { }"

let maximalExample =
        assert
      :   graph
            { strict = True
            , directionality = types.Directionality.digraph
            , id = Some "A"
            , statements =
              [ types.statement.node
                  { nodeID =
                    { id = "Node #0"
                    , port = Some (types.Port.CompassPoint types.CompassPoint.n)
                    }
                  , attributes = toMap { color = "red" }
                  }
              , types.statement.node
                  { nodeID = { id = "Node #1", port = None types.Port }
                  , attributes = [] : List types.Attribute
                  }
              , types.statement.edges
                  { vertices =
                    [ types.vertex.nodeID
                        { id = "Node #0"
                        , port = Some
                            (types.Port.CompassPoint types.CompassPoint.n)
                        }
                    , types.vertex.nodeID
                        { id = "Node #1", port = None types.Port }
                    , types.vertex.subgraph
                        ( types.subgraph
                            { id = Some "Subgraph #0"
                            , statements =
                              [ types.statement.node
                                  { nodeID =
                                    { id = "Subgraph #0 - Node #0"
                                    , port = None types.Port
                                    }
                                  , attributes = [] : List types.Attribute
                                  }
                              , types.statement.node
                                  { nodeID =
                                    { id = "Subgraph #0 - Node #1"
                                    , port = None types.Port
                                    }
                                  , attributes = [] : List types.Attribute
                                  }
                              ]
                            }
                        )
                    ]
                  , attributes = toMap { label = "Label #0" }
                  }
              , types.statement.subgraph
                  ( types.subgraph
                      { id = Some "Subgraph #1"
                      , statements =
                        [ types.statement.node
                            { nodeID =
                              { id = "Subgraph #1 - Node #0"
                              , port = None types.Port
                              }
                            , attributes = [] : List types.Attribute
                            }
                        , types.statement.node
                            { nodeID =
                              { id = "Subgraph #1 - Node #1"
                              , port = None types.Port
                              }
                            , attributes = [] : List types.Attribute
                            }
                        ]
                      }
                  )
              ]
            }
        ≡ ''
          strict digraph "A" { "Node #0":n [ "color" = "red" ]; "Node #1"; "Node #0":n -> "Node #1" -> subgraph "Subgraph #0" { "Subgraph #0 - Node #0"; "Subgraph #0 - Node #1"; } [ "label" = "Label #0" ]; subgraph "Subgraph #1" { "Subgraph #1 - Node #0"; "Subgraph #1 - Node #1"; }; }''

in  { graph }
