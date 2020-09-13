module Patterns where 

type Point = (Int, Int)

data Cell = Alive | Dead deriving (Eq,Show)

type Image = Point -> Cell

--Some well known patterns

glider :: Image
glider (0,1) = Alive
glider (1,2) = Alive
glider (2,2) = Alive
glider (2,1) = Alive
glider (2,0) = Alive
glider _ = Dead


block :: Image
block (0,0) = Alive
block (1,0) = Alive
block (1,1) = Alive
block (0,1) = Alive
block _ = Dead

toad :: Image
toad (1,1) = Alive
toad (1,2) = Alive
toad (1,3) = Alive
toad (2,2) = Alive
toad (2,3) = Alive
toad (2,4) = Alive
toad _ = Dead