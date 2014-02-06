module Dijkstra
where
  
  -- shortest paths using Dijkstra's algorithm

import Data.Function
import Data.Map hiding (deleteMin)
import Leftist hiding (insert)
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


data AdjacencyList = Map Vertex [Vertex] -- have to build this



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
                     (addNeighbors delta v u')
       addNeighbors :: Double -> Vertex -> Unknowns -> Unknowns
       addNeighbors = error "unimplemented"

       neighborsOfStartNode = addNeighbors 0.0 start emptyHeap
