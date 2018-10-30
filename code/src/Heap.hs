module Heap (
    Heap(..), Addr,
    hInitial,
    hAlloc
) where

import Data.List(map, elemIndex, lookup, foldr)
import qualified Data.List as L

import Data.Map.Strict
import qualified Data.Map.Strict as Map

import Data.Maybe
{-
 -  Heap is represented as a triple:
 -      * number of objects in the heap
 -      * list of unused addresses
 -      * dictionary of addresses to heap objects
 -  Addresses are numbers
 -}
type Heap a = (Int, [Int], Map.Map Int a)
type Addr = Int
hNull :: Addr
hNull = -1
hIsnull :: Heap a -> Addr -> Bool
hIsnull (_, _, cts) a = 
    case Map.lookup a cts of
        Nothing -> True
        Just _ -> False
-----------------------------------------------------------------------------------
-------------------------HEAP CONSTRUCTION OPERATIONS------------------------------
-----------------------------------------------------------------------------------
-- Initialize heap
hInitial :: Heap a
hInitial = (0, [1..], Map.empty)

-- Allocate a new a heap object
--  Returns the new heap and the address for the new object
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, next : free, cts) n = ((size + 1, free, Map.insert next n cts), next)

-- Given an address a, update object with value n
--  Returns the new heap
hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, Map.insert a n (Map.delete a cts))

-- Free address a
-- Returns the new heap
hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size - 1, a : free, Map.delete a cts)

-----------------------------------------------------------------------------------
-------------------------------SEARCH OPERATIONS-----------------------------------
-----------------------------------------------------------------------------------
-- Returns content of a
hLookup :: Heap a -> Addr -> a
hLookup heap@(_, _, cts) a =
    if hIsnull heap a then error ("Address: " ++ show a ++ " not in the heap") 
    else fromJust (Map.lookup a cts)

-- Returns all addresses in the heap
hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = Map.keys cts

-- Returns size of the heap
hSize :: Heap a -> Int
hSize (size, _, _) = size
----------------------------------------------------------------------------------
---------------------------------PRINT ADDRESSES----------------------------------
----------------------------------------------------------------------------------
showAddr :: Addr -> String
showAddr a = "#" ++  show a