module Dijkstra
where
  
  -- shortest paths using Dijkstra's algorithm

import Data.Function
import Data.Map hiding (deleteMin, foldl)
import Data.Maybe
import Leftist hiding (insert)
import Prelude hiding (lookup)
import qualified Leftist

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap = Leftist.insert
emptyHeap = Leftist.Empty

newtype Uid = Uid String -- OSM identifier of a node
              deriving (Eq, Ord)

data GPS = GPS { lat :: Double, lon :: Double }
data Vertex = Vertex { uid :: Uid, pos :: GPS }
type Way = [Uid] -- representation from Open Street Map

data InputGraph = OSM { nodes :: [Vertex], ways :: [Way] }


type AdjacencyList = Map Vertex [Vertex] -- have to build this



instance Eq Vertex where
  (==) = (==) `on` uid

instance Ord Vertex where
  compare = compare `on` uid
  
-----------------------------------

-- | label for a node in the graph
data KnownData = Start
               | Known { me :: Vertex, distance :: Double, parent :: KnownData }
                   -- ^ distance is length of shortest path to start node
                   --   parent is first step on that path
                 
type Solution = Map Vertex KnownData

data Unknown = Unknown { me' :: Vertex, distance' :: Double, parent' :: KnownData }

instance Eq Unknown where
  (==) = (==) `on` distance'

instance Ord Unknown where
  compare = compare `on` distance'

type Unknowns = Heap Unknown


solve :: AdjacencyList -> Vertex -> Solution 
solve adj start = steps (insert start Start empty) neighborsOfStartNode
 where steps :: Solution -> Unknowns -> Solution
       steps solution unknowns =
         case deletemin unknowns of
           Nothing -> solution
           Just (Unknown v delta parent, u') ->
             if v `member` solution then
               -- already found the shortest path to this node
               steps solution u'
             else
               steps (insert v (Known v delta parent) solution)
                     (addNeighbors (Known v delta parent) v u')
       addNeighbors :: KnownData -> Vertex -> Unknowns -> Unknowns
       addNeighbors parent me unknowns =
         foldl add unknowns (fromJust $ lookup me adj)
         where add unknowns v =
                 insertHeap
                 (Unknown v (dist parent + (gpsDistance `on` pos) me v) parent)
                 unknowns
                                        
       neighborsOfStartNode = addNeighbors Start start emptyHeap

       dist :: KnownData -> Double
       dist Start = 0.0
       dist (Known { distance = d }) = d


gpsDistance :: GPS -> GPS -> Double
gpsDistance = error "some number of meters"
