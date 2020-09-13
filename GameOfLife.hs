module GameOfLife where

{- An implementation of Conway's game of life in Haskell.
   Rules:
    Any live cell with two or three live neighbours survives.
    Any dead cell with three live neighbours becomes a live cell.
    All other live cells die in the next generation. Similarly, all other dead cells stay dead.
-}

import Patterns
import Control.Concurrent (threadDelay)
import Control.Monad (mapM_) 
import System.Process
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

genNeighbours :: Point -> [Point]
genNeighbours (x,y) = [(x+xn,y+yn)| xn<-[-1,0,1], yn<-[-1,0,1], (xn,yn) /= (0,0)]

count :: Image -> Point -> Int
count image = length . filter isAlive . map image . genNeighbours

evolve :: Image -> Image
evolve current pos
  | cellAlive && liveNeighbours `elem` [2,3] = Alive
  | (not cellAlive) && liveNeighbours == 3 = Alive 
  | otherwise = Dead
  where liveNeighbours = count current pos 
        cellAlive = isAlive (current pos)

type Display color = Cell -> color

displayText Alive = "X"
displayText Dead = " "

displayCell :: Display color -> Image -> Point -> color
displayCell render image p = render (image p)

displayTextCell = displayCell displayText 

displayLine generation p = concatMap (displayTextCell generation) [(p,y)| y<- [0..10]]

displayGame generation = map (displayLine generation) [0..10]

playGame :: Image -> IO()
playGame image = do
    system "clear"
    mapM_ (C.putStrLn . encodeUtf8 . T.pack) (displayGame image)
    threadDelay 500000
    playGame (evolve image)

main :: IO()
main = do
    playGame toad 
    return ()
