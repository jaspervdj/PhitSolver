module Main where

import Prelude as P
import Data.Set as S
import Control.Monad
import Data.List (groupBy)

data Tile = Tile { tileX :: Int
                 , tileY :: Int
                 } deriving (Eq, Ord, Show)

type Piece = Set Tile
type Board = Piece

dimension :: Piece -> (Tile -> Int) -> Int
dimension piece f = (findMax xs)
    where xs = S.map f piece

width :: Piece -> Int
width piece = dimension piece tileX

height :: Piece -> Int
height piece = dimension piece tileY

validPositions :: Board -> Piece -> [Piece]
validPositions board piece = P.filter (canPutPiece board) positions
    where positions = P.map (\(x, y) -> translate piece x y) coords
          coords = [(x, y) | x <- [0 .. (width board) - (width piece)],
                             y <- [0 .. (height board) - (height piece)]]
          translate piece x y = S.map (translate' x y) piece
          translate' x y tile = Tile (tileX tile + x)
                                 (tileY tile + y)
          canPutPiece board piece = all (flip member board) (toList piece)


solve :: Board -> [Piece] -> [Piece] -> Maybe [Piece]
solve board left added | P.null left = Just added
                       | otherwise         = (foldr mplus Nothing) solutions
    where solutions = P.map solve' positions
          solve' piece = solve (putPiece board piece) (tail left) (piece:added)
          positions = validPositions board (head left)
          putPiece board piece = board \\ piece

loadPiece :: [String] -> Piece
loadPiece lines = fromList $ P.map toTile coords
    where coords = P.filter isTile $ concat $ P.map numberCols numberLines
          toTile (c, x, y) = Tile x y
          isTile (c, x, y) = c == '#'
          numberLines = zip lines [1..]
          numberCols (line, lineNumber) = zipWith (\c -> \n -> (c, n, lineNumber)) line [1..]

pieces :: [String] -> [[String]]
pieces = removeCrap . groupBy (\a b -> b /= "")
    where removeCrap = P.filter (not . P.null) . P.map (P.filter (\a -> a /= ""))

renderSolution :: Board -> [Piece] -> String
renderSolution board solution = unlines $ P.map renderRow $ splitRows finalRender
    where renderRow row = P.map (\(c, x, y) -> c) row
          splitRows = groupBy (\(c1, x1, y1) (c2, x2, y2) -> (y1 == y2))
          finalRender = foldr renderPiece initialRender (zip solution characters)
          initialRender = [(' ', x, y) | y <- rows, x <- columns]
          columns = [1 .. (width board)]
          rows = [1 .. (height board)]
          renderPiece (piece, char) r = P.map (renderTile char piece) r
          renderTile char piece (c, x, y) = if Tile x y `member` piece then (char, x, y)
                                                                       else (c, x, y)
          characters = ['a' .. 'z'] ++ ['0' .. '9'] ++ ['A' .. 'Z']

main = do
    inp <- getContents
    let ps = P.map loadPiece (pieces $ lines inp)
        board = head ps
        availablePieces = tail ps
        solution = solve board availablePieces []
    putStrLn $ case solution of Nothing -> "No solution found."
                                Just s  -> renderSolution (head ps) s
