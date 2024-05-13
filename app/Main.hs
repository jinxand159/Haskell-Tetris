{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Blank
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import System.Random

type Board = [[Bool]]
data Piece = Piece { pieceShape :: [[Bool]], pieceX :: Int, pieceY :: Int }

main :: IO ()
main = blankCanvas 3000 $ \context -> do
    let rows = 20
        cols  = 10
        cellSize = 30
    gameLoop context initialBoard

initialBoard :: Board
initialBoard = replicate 20 (replicate 10 False)

gameLoop :: DeviceContext -> Board -> IO ()
gameLoop context board = do
    let piece = Piece { pieceShape = [[True, True], [True, True]], pieceX = 4, pieceY = 0 }
    drawBoard context board
    drawPiece context piece
    threadDelay (1000 * 1000)
    gameLoop context (updateBoard board piece)

drawBoard :: DeviceContext -> Board -> IO ()
drawBoard context board = send context $ do
    clearRect (0, 0, 300, 600)
    strokeStyle "white"
    lineWidth 2
    let rows = 20
        cols = 10
        cellSize = 30
    sequence_ [do
        beginPath()
        moveTo (fromIntegral x * fromIntegral cellSize, 0)
        lineTo (fromIntegral x * fromIntegral cellSize, fromIntegral (rows * fromIntegral cellSize))
        stroke()
        | x <- [0..cols-1]]
    sequence_ [do
        beginPath()
        moveTo (0, fromIntegral y * fromIntegral cellSize)
        lineTo (fromIntegral (cols * fromIntegral cellSize), fromIntegral y * fromIntegral cellSize)
        stroke()
        | y <- [0..rows-1]]
    fillStyle "blue"
    sequence_ [do
        sequence_ [do
            when (board !! y !! x) $
                fillRect (fromIntegral x * fromIntegral cellSize, fromIntegral y * fromIntegral cellSize, fromIntegral cellSize, fromIntegral cellSize)
            | x <- [0..cols-1]]
        | y <- [0..rows-1]]

drawPiece :: DeviceContext -> Piece -> IO ()
drawPiece context piece = send context $ do
    let cellSize = 10
    fillStyle "red"
    sequence_ [do
        sequence_ [do
            when (pieceShape piece !! (y - pieceY piece) !! (x - pieceX piece)) $
                fillRect (fromIntegral x * fromIntegral cellSize, fromIntegral y * fromIntegral cellSize, fromIntegral cellSize, fromIntegral cellSize)
            | x <- [pieceX piece..pieceX piece+1]]
        | y <- [pieceY piece..pieceY piece+1]]

updateBoard :: Board -> Piece -> Board
updateBoard board piece = foldl (\acc (y,row) -> updateRow y row acc) board (zip [0..] (pieceShape piece))

updateRow :: Int -> [Bool] -> Board -> Board
updateRow y row board
    | y < 0 = board
    | otherwise = take y board ++ [updatedRow] ++ drop (y + 1) board
    where
        updatedRow = zipWith (||) row (board !! y)
