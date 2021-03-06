module Main where

import Prelude as P
import Data.Set as S
import Control.Monad
import Data.List (groupBy)
import Data.Char

data Tile = Tile { tileX :: Int
                 , tileY :: Int
                 } deriving (Eq, Ord, Show)

type Piece = Set Tile
type Board = Piece
type Solution = [Piece]

dimension :: (Tile -> Int) -> Piece -> Int
dimension f piece = (findMax xs)
    where xs = S.map f piece

width :: Piece -> Int
width = dimension tileX

height :: Piece -> Int
height = dimension tileY

validPositions :: Board -> Piece -> [Piece]
validPositions board piece = P.filter (canPutPiece board) allPositions
    where allPositions :: [Piece]
          allPositions = P.map (translate piece) allCoords

          allCoords :: [(Int, Int)]
          allCoords = [(x, y) | x <- [0 .. (width board) - (width piece)],
                                y <- [0 .. (height board) - (height piece)]]

          translate :: Piece -> (Int, Int) -> Piece
          translate piece (x, y) = S.map (\(Tile tx ty) -> Tile (tx + x) (ty + y)) piece

          canPutPiece :: Board -> Piece -> Bool
          canPutPiece board piece = all (`member` board) (toList piece)


solve :: Board -> [Piece] -> [Piece] -> Maybe Solution
solve board piecesLeft added | P.null piecesLeft = Just added
                             | otherwise         = foldr mplus Nothing solutions
    where solutions :: [Maybe Solution]
          solutions = P.map solve' positions

          solve' :: Piece -> Maybe Solution
          solve' piece = solve (putPiece board piece) (tail piecesLeft) (piece:added)

          positions :: [Piece]
          positions = validPositions board (head piecesLeft)

          putPiece :: Board -> Piece -> Board
          putPiece board piece = board \\ piece

loadPiece :: [String] -> Piece
loadPiece lines = fromList $ P.map toTile coords
    where coords = P.filter isTile $ concat $ P.map numberCols numberLines
          toTile (c, x, y) = Tile x y
          isTile (c, x, y) = not $ isSpace c
          numberLines = zip lines [1 ..]
          numberCols (line, lineNumber) = zipWith (\c n -> (c, n, lineNumber)) line [1 ..]

pieces :: [String] -> [[String]]
pieces = removeCrap . groupBy (\a b -> b /= "")
    where removeCrap = P.filter (not . P.null) . P.map (P.filter (/= ""))

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
          characters = concat $ repeat $ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['A' .. 'Z']

solvePhit :: String -> String
solvePhit input = case solution of Nothing -> "No solution found."
                                   Just s  -> renderSolution board s
    where ps = P.map loadPiece(pieces $ lines input)
          board = head ps
          availablePieces = tail ps
          solution = solve board availablePieces []

main = interact solvePhit
