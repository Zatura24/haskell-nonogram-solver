-- module Nonogram (nonogram) where

import Control.Monad
import Data.List
import Data.Maybe

-- Given information about the grid
type RowInfo    = [[Int]]
type ColumnInfo = [[Int]]

-- Building stones of the puzzle
type Row s = [s]
type Grid s = [Row s]
-- Partial information about a square
type Square = Maybe Bool

-- If there are any solutions, print the first. Otherwise something is wrong with the information
nonogram :: RowInfo -> ColumnInfo -> String
nonogram rowInfo columnInfo = case solve rowInfo columnInfo of 
            []       -> "Inconsistent\n"
            (grid:_) -> gridToFormattedStringRepresentation rowInfo columnInfo grid

-- Find a list with all the solutions to the nonogram
solve :: RowInfo -> ColumnInfo -> [Grid Bool]
solve rowInfo columnInfo = 
                [ grid' |
                        -- Deduce as many squares as we can
                grid <- maybeToList $ deduction rowInfo columnInfo,
                        -- Guess the rest, governed by rowInfo
                grid' <- zipWithM (rowsMatching $ length columnInfo) rowInfo grid,
                        -- Check each guess against columnInfo
                map contract (transpose grid') == columnInfo]
    where   
        contract = map length . filter head . group

-- A nonogram with all the values we can deduce
deduction :: RowInfo -> ColumnInfo -> Maybe (Grid Square)
deduction rowInfo columnInfo = converge step init
    where   
        lengthRow       = length rowInfo
        lengthColumn    = length columnInfo
        init            = replicate lengthRow (replicate lengthColumn Nothing)
        step            = 
            (improve lengthColumn rowInfo . transpose) 
                <&> 
            (improve lengthRow columnInfo . transpose)
        improve n       = zipWithM (common n)
        (f <&> g) x     = f =<< g x

-- repeatedly apply f until a fixed point is reached
converge :: (Monad m, Eq a) => (a -> m a) -> a -> m a
converge f s = do
        s' <- f s
        if s' == s then return s else converge f s'

-- common n ks partial = commonality between all possible ways of
-- placing blocks of length ks in a row of length n that match partial.
common :: Int -> [Int] -> Row Square -> Maybe (Row Square)
common n ks partial = case rowsMatching n ks partial of
        [] -> Nothing
        rs -> Just (foldr1 (zipWith unify) (map (map Just) rs))
    where   unify :: Square -> Square -> Square
            unify x y
                | x == y = x
                | otherwise = Nothing

-- rowsMatching n ks partial = all possible ways of placing blocks of
-- length ks in a row of length n that match partial.
rowsMatching :: Int -> [Int] -> Row Square -> [Row Bool]
rowsMatching n [] [] = [[]]
rowsMatching n ks [] = []
rowsMatching n ks (Nothing:partial) =
        rowsMatchingAux n ks True partial ++
        rowsMatchingAux n ks False partial
rowsMatching n ks (Just s:partial) = 
        rowsMatchingAux n ks s partial

rowsMatchingAux :: Int -> [Int] -> Bool -> Row Square -> [Row Bool]
rowsMatchingAux n ks False partial =
        [False : row | row <- rowsMatching (n-1) ks partial]
rowsMatchingAux n [k] True partial =
        [replicate k True ++ replicate (n-k) False |
                n >= k && all (/= Just False) front && all (/= Just True) back]
    where (front, back) = splitAt (k-1) partial
rowsMatchingAux n (k:ks) True partial =
        [replicate k True ++ False : row |
                n > k+1 && all (/= Just False) front && blank /= Just True,
                row <- rowsMatching (n-k-1) ks partial']
    where (front, blank:partial') = splitAt (k-1) partial

gridToFormattedStringRepresentation :: RowInfo -> ColumnInfo -> Grid Bool -> String
gridToFormattedStringRepresentation rowInfo columnInfo ss = unlines (zipWith showRow rowInfo ss ++ showCols columnInfo)
    where   
        showRow rowInfo ss = concat [[' ', cellChar s] | s <- ss] ++ "|  " ++ unwords (map show rowInfo)
        showCols columnInfo
            | all null columnInfo = []
            | otherwise = concatMap showCol columnInfo : showCols (map advance columnInfo)
        showCol (k:_)
            | k < 10 = ' ':show k
            | otherwise = show k
        showCol [] = "  "
        cellChar True = '■'
        cellChar False = ' '
        advance [] = []
        advance (x:xs) = xs

main :: IO ()
-- main = putStr $ nonogram [[2,1],[1,3],[1,2],[3],[4],[1]] [[1],[5],[2],[5],[2,1],[2]]
main = putStr $ nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
-- main = putStr $ nonogram [[1,7],[1,6],[2,6],[3,5],[4,5],[3,4],[3,4],[2,1,3],[2,2,3],[1,3,1],[1,4],[5,1]] [[1,8],[8],[5,1],[1,1,3],[3,5],[5,4],[7,2],[9],[10],[9,1]]
-- main = putStr $ nonogram [[7],[11],[3,3],[3,3],[4,2],[3,2],[2,1,2],[3,6,1],[2,2,1,2],[1,1,6,1],[2,2,1,2],[3,2,2,2],[3,2,4,1,1],[2,4,1,1,2,2,1],[2,12,2,1],[1,1,15,1],[1,2,2,6,2,1],[5,5,1,1],[4,5,3],[3,6,2],[3,1,5,1],[2,1,2,2,2],[1,5,1,2],[1,5,1,2],[7,1,2]] [[6,6,7],[6,4,5,1],[4,1,3,5,3],[3,2,4,4],[2,1,3],[1,2,4],[2,2,1,6],[2,6,3],[2,12],[2,12],[2,7],[2,2,8],[2,1,4,5],[2,1,3,3],[2,3,1,2],[2,9],[1,1,1,2,2],[2,2,1,2],[2,1,1,1],[2,2,3],[3,3],[2,2,2],[2,1,4],[2,2],[6]]