{-# LANGUAGE OverloadedStrings #-}
module Text.GEXF
    (
      toDocument
    ) where

import Data.Graph.Inductive
import Text.XML
import Data.Text hiding ( map, zipWith, group, length )
import Data.Map hiding ( map )

toDocument :: (Graph graph, Ord edge) =>
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
        , "xmlns:viz" .= "http://www.gexf.net/1.2draft/viz"
        , "xmlns:xsi" .= "http://www.w3.org/2001/XMLSchema-instance"
        , "xsi:schemaLocation" .= "http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd"
        , "version" .= "1.2"
        ]
        [ NodeElement $ element "graph"
          [ "mode" .= "static"
          , "defaultedgetype" .= "directed"
          ]
          [ NodeElement $ element "nodes" [] $
            map (nodeToNode graph toLabel) $ labNodes graph
          , NodeElement $ element "edges" [] $
            zipWith edgeToNode [0..] $ labEdges graph
          ]
        ]
    , documentEpilogue = []
    }

a .= b = ( Name a Nothing Nothing, b )

element name attrs elems =
    Element (Name name Nothing Nothing) (fromList attrs) elems

nodeToNode graph toLabel (node, a) =
    NodeElement $ element "node"
    [ "id" .= pack (show node)
    , "label" .= toLabel a
    ]
    [ NodeElement $ element "viz:size"
      [ "value" .= pack (show $ 1 + log (1 + (toEnum $ length $ pre graph node)))
      ]
      []
    ]

edgeToNode n (src, dst, a) =
    NodeElement $ element "edge"
    [ "id" .= pack (show n)
    , "source" .= pack (show src)
    , "target" .= pack (show dst)
    ]
    []
