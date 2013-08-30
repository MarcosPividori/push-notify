
module Connect4
    ( emptyBoard
    , newMovement
    , checkWinner
    , Board (..)
    ) where

import Data.Array

type Board = Array (Int,Int) Int

emptyBoard :: Board
emptyBoard = array ((0,0),(5,6)) [(i, 0) | i <- range ((0,0),(5,6))]

newMovement :: Int -> Int -> Board -> Board 
newMovement pos player arr = if arr!(0,pos) /= 0
                              then arr
                              else loop 0
                                where  loop i = if i == 5
                                                 then arr // [((5,pos),player)]
                                                 else if arr!(i+1,pos) /= 0
                                                       then arr // [((i,pos),player)]
                                                       else loop (i+1)

checkWinner :: Board -> Maybe Int
checkWinner arr = checkDiagonals1 arr

checkDiagonals1 arr = loop 0
                  where 
                       loop1 n i j res 
                                   | res == 4  = Just 1
                                   | res == -4 = Just (-1)
                                   | otherwise = if i>5 || j > 6
                                                  then loop (n+1)
                                                  else
                                                   if arr!(i,j) *res > 0
                                                    then loop1 n (i+1) (j+1) (res+arr!(i,j))
                                                    else loop1 n (i+1) (j+1) (arr!(i,j))
                       loop n 
                            | n == 0    = loop1 n 2 0 0
                            | n == 1    = loop1 n 1 0 0
                            | n == 2    = loop1 n 0 0 0
                            | n == 3    = loop1 n 0 1 0
                            | n == 4    = loop1 n 0 2 0
                            | otherwise = checkDiagonals2 arr

checkDiagonals2 arr = loop 0
                  where 
                       loop1 n i j res 
                                   | res == 4  = Just 1
                                   | res == -4 = Just (-1)
                                   | otherwise = if i<0 || j > 6
                                                  then loop (n+1)
                                                  else
                                                   if arr!(i,j) *res > 0
                                                    then loop1 n (i-1) (j+1) (res + arr!(i,j))
                                                    else loop1 n (i-1) (j+1) (arr!(i,j))
                       loop n 
                            | n == 0    = loop1 n 3 0 0
                            | n == 1    = loop1 n 4 0 0
                            | n == 2    = loop1 n 5 0 0
                            | n == 3    = loop1 n 5 1 0
                            | n == 4    = loop1 n 5 2 0
                            | n == 5    = loop1 n 5 3 0
                            | otherwise = checkColumns arr

checkColumns arr = loop 0
                  where 
                       loop1 i n res 
                                   | res == 4  = Just 1
                                   | res == -4 = Just (-1)
                                   | otherwise = if n > 5
                                                  then loop (i+1)
                                                  else
                                                   if arr!(n,i) *res > 0
                                                    then loop1 i (n+1) (res + arr!(n,i))
                                                    else loop1 i (n+1) (arr!(n,i))
                       loop i = if i > 6
                                     then checkRows arr
                                     else loop1 i 0 0 

checkRows arr = loop 0
                  where 
                       loop1 i n res 
                                   | res == 4  = Just 1
                                   | res == -4 = Just (-1)
                                   | otherwise = if n > 6
                                                  then loop (i+1)
                                                  else
                                                   if arr!(i,n) *res > 0
                                                    then loop1 i (n+1) (res + arr!(i,n))
                                                    else loop1 i (n+1) (arr!(i,n))
                       loop i = if i > 5
                                     then Nothing
                                     else loop1 i 0 0 
