let types = ./types.dhall

let schemas = ./schemas.dhall

let render = ./render.dhall

let dhall-dot = types ⫽ schemas ⫽ { render }

let minimalExample =
        assert
      :   dhall-dot.render
            { strict = False
            , directionality = dhall-dot.Directionality.graph
            , id = None dhall-dot.ID
            , statements = [] : List dhall-dot.Statement
            }
        ≡ ''
          graph { }
          ''

let maximalExample =
        assert
      :   dhall-dot.render
            { strict = True
            , directionality = dhall-dot.Directionality.digraph
            , id = Some "A"
            , statements =
              [ dhall-dot.statement.node
                  { nodeID =
                    { id = "Node #0"
                    , port = Some
                        (dhall-dot.Port.CompassPoint dhall-dot.CompassPoint.n)
                    }
                  , attributes = toMap { color = "red" }
                  }
              , dhall-dot.statement.node
                  { nodeID = { id = "Node #1", port = None dhall-dot.Port }
                  , attributes = [] : List dhall-dot.Attribute
                  }
              , dhall-dot.statement.edges
                  { vertices =
                    [ dhall-dot.vertex.nodeID
                        { id = "Node #0"
                        , port = Some
                            ( dhall-dot.Port.CompassPoint
                                dhall-dot.CompassPoint.n
                            )
                        }
                    , dhall-dot.vertex.nodeID
                        { id = "Node #1", port = None dhall-dot.Port }
                    , dhall-dot.vertex.subgraph
                        ( dhall-dot.subgraph
                            { id = Some "cluster_0"
                            , attributes = toMap { style = "filled" }
                            , statements =
                              [ dhall-dot.statement.node
                                  { nodeID =
                                    { id = "Subgraph #0 - Node #0"
                                    , port = None dhall-dot.Port
                                    }
                                  , attributes = [] : List dhall-dot.Attribute
                                  }
                              , dhall-dot.statement.node
                                  { nodeID =
                                    { id = "Subgraph #0 - Node #1"
                                    , port = None dhall-dot.Port
                                    }
                                  , attributes = [] : List dhall-dot.Attribute
                                  }
                              ]
                            }
                        )
                    ]
                  , attributes = toMap { label = "Label #0" }
                  }
              , dhall-dot.statement.subgraph
                  ( dhall-dot.subgraph
                      { id = Some "Subgraph #1"
                      , attributes = [] : List dhall-dot.Attribute
                      , statements =
                        [ dhall-dot.statement.node
                            { nodeID =
                              { id = "Subgraph #1 - Node #0"
                              , port = None dhall-dot.Port
                              }
                            , attributes = [] : List dhall-dot.Attribute
                            }
                        , dhall-dot.statement.node
                            { nodeID =
                              { id = "Subgraph #1 - Node #1"
                              , port = None dhall-dot.Port
                              }
                            , attributes = [] : List dhall-dot.Attribute
                            }
                        ]
                      }
                  )
              ]
            }
        ≡ ''
          strict digraph "A" { "Node #0":n [ "color" = "red" ]; "Node #1"; "Node #0":n -> "Node #1" -> subgraph "cluster_0" { "style" = "filled"; "Subgraph #0 - Node #0"; "Subgraph #0 - Node #1"; } [ "label" = "Label #0" ]; subgraph "Subgraph #1" { "Subgraph #1 - Node #0"; "Subgraph #1 - Node #1"; }; }
          ''

in  dhall-dot
