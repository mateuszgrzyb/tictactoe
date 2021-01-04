module Board where

import Data.Array

someFunc :: IO ()
someFunc = putStrLn "someFunc"

generate :: (Ix i) => (i,i) -> (i -> e) -> Array i e
generate bnds f = array bnds $ map (\i -> (i, f i)) $ range bnds

type Pos = (Int, Int)

data Field
    = Empty
    | Cross
    | Circle
    deriving (Eq)

data Player
    = Player1 
    | Player2
    deriving (Show)

getField :: Player -> Field
getField Player1 = Cross
getField Player2 = Circle

next :: Player -> Player
next Player1 = Player2
next Player2 = Player1

instance Show Field where
    show Empty  = "_"
    show Cross  = "X"
    show Circle = "O"

newtype Board = Board (Array Pos Field)

instance Show Board where
    show b@(Board arr) = concat [show [ arr!(i,j) | i <- idx ] ++ "\n" | j <- idx ]
        where idx = [1..size b]

emptyBoard :: Int -> Board
emptyBoard n = Board $ generate ((1,1),(n,n)) $ const Empty

set :: Board -> Pos -> Field -> Board
set (Board arr) pos e = Board $ arr//[(pos,e)]

isFull :: Board -> Bool
isFull (Board arr) = Empty `notElem` elems arr

check :: Board -> Player -> Bool
check b@(Board arr) p = any full' $ diags arr <> rows arr <> cols arr
  where
    full' = all (== f)
    diags b = [[ b!(i,i)     | i <- idx ],
               [ b!(i,n-i+1) | i <- idx ]]
    rows b  = [[ b!(i,j) | i <- idx ] | j <- idx ]
    cols b  = [[ b!(j,i) | i <- idx ] | j <- idx ]
    idx = [1..n]
    n = size b
    f = getField p

size :: Board -> Int
size (Board b) = snd $ snd $ bounds b

isEmpty :: Board -> Pos -> Bool
isEmpty (Board arr) p = arr!p == Empty