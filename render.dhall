let Text/concatSep =
      https://prelude.dhall-lang.org/v17.1.0/Text/concatSep.dhall sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let Text/concatMapSep =
      https://prelude.dhall-lang.org/v17.1.0/Text/concatMapSep.dhall sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let List/map =
      https://prelude.dhall-lang.org/v17.1.0/List/map.dhall sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let List/null =
      https://prelude.dhall-lang.org/v17.1.0/List/null.dhall sha256:2338e39637e9a50d66ae1482c0ed559bbcc11e9442bfca8f8c176bbcd9c4fc80

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

let graphAttributeList =
      λ(`as` : List types.Attribute) →
        "${Text/concatMapSep "; " types.Attribute attribute `as`};"

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
              λ ( x
                : { id : Optional Text
                  , attributes : List types.Attribute
                  , statements : List Text
                  }
                ) →
                let renderedID =
                      merge
                        { None = [] : List Text
                        , Some = λ(i : types.ID) → [ "subgraph", id i ]
                        }
                        x.id

                let renderedAttributeList =
                      if    List/null types.Attribute x.attributes
                      then  [] : List Text
                      else  [ graphAttributeList x.attributes ]

                in  Text/concatSep
                      " "
                      (   renderedID
                        # [ "{" ]
                        # renderedAttributeList
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

        in      Text/concatSep
                  " "
                  (   renderedStrict
                    # [ directionality g.directionality ]
                    # renderedID
                    # [ "{" ]
                    # renderedStatements
                    # [ "}" ]
                  )
            ++  "\n"

in  graph
