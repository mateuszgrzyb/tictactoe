
module Game where

import Board
import Text.Read
import Data.Ix
import Data.Char
import Data.Function


data GameState = GameState 
    { getBoard :: Board 
    , getWinner :: Maybe Player 
    }

create :: IO Board
create = do
    print "Enter size of board:"
    getLine >>= returnCreate . readMaybe
  where
    returnCreate Nothing  = print "Not a number" >> create
    returnCreate (Just n) = return $ emptyBoard n


readPos :: Board -> String -> Maybe Pos
readPos b pos | [x,y] <- pos
              , inRange ('a',end) x && inRange ('1', intToDigit n) y
              = return (ord x - ord 'a' + 1, digitToInt y)
              | otherwise = Nothing
  where
    end = chr $ ord 'a' + n
    n   = size b

getPos :: (Board -> IO Pos) -> Board -> IO Pos
getPos rec b = do
    print b
    print "Choose position"
    getLine >>= returnPos . readPos b
  where
    returnPos pos | Just p <- pos
                  , b `isEmpty` p 
                  = return p 
                  | otherwise = print "Not a position" >> rec b


loop:: (Board -> Player -> IO GameState) -> Board -> Player -> IO GameState
loop rec board player = do
    pos <- fix getPos board
    checkLoop (set board pos $ getField player)
  where
    checkLoop newboard | check newboard player = return $ GameState newboard $ Just player
                       | isFull newboard       = return $ GameState newboard Nothing
                       | otherwise             = rec newboard $ next player


game :: IO ()
game = do
    board <- create
    let player = Player1
    state <- fix loop board player
    print $ getBoard state
    print $ getWinner state

