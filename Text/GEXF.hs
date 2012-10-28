{-# LANGUAGE OverloadedStrings #-}
module Text.GEXF
    (
      toDocument
    ) where

import Data.Graph.Inductive
import Text.XML
import Data.Text hiding ( map, zipWith )
import Data.Map hiding ( map )

toDocument :: Graph graph =>
              (node -> Text)
           -> graph node edge
           -> Document
toDocument toLabel graph =
    Document
    { documentPrologue =
          Prologue
          { prologueBefore = []
          , prologueDoctype = Nothing
          , prologueAfter = []
          }
    , documentRoot =
        element "gexf"
        [ "xmlns" .= "http://www.gexf.net/1.2draft"
        , "version" .= "1.2"
        ]
        [ NodeElement $ element "graph"
          [ "mode" .= "static"
          , "defaultedgetype" .= "directed"
          ]
          [ NodeElement $ element "nodes" [] $
            map (nodeToNode toLabel) $ labNodes graph
          , NodeElement $ element "edges" [] $
            zipWith edgeToNode [0..] $ labEdges graph
          ]
        ]
    , documentEpilogue = []
    }

a .= b = ( Name a Nothing Nothing, b )

element name attrs elems =
    Element (Name name Nothing Nothing) (fromList attrs) elems

nodeToNode toLabel (node, a) =
    NodeElement $ element "node"
    [ "id" .= pack (show node)
    , "label" .= toLabel a
    ]
    []

edgeToNode n (src, dst, a) =
    NodeElement $ element "edge"
    [ "id" .= pack (show n)
    , "source" .= pack (show src)
    , "target" .= pack (show dst)
    ]
    []
