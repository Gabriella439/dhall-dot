let types = ./types.dhall

let NodeID = { Type = types.NodeID, default.port = None types.Port }

let Graph =
      { Type = types.Graph
      , default =
        { strict = False
        , id = None types.ID
        , statements = [] : List types.Statement
        }
      }

in  { NodeID, Graph }
