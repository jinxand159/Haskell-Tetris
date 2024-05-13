{-# LANGUAGE OverloadedStrings #-}

module DrawGrid where
import Graphics.Blank
import Control.Monad
import Data.Text
import Debug.Trace

drawBackground :: DeviceContext -> Canvas ()
drawBackground context = do
  fillStyle "#1a1a1a"
  fillRect (0, 0, width context, height context)
  strokeStyle "rgba(153, 153, 153, 0.8)"
  lineWidth gridLineWidth
  stroke ()

drawGrid :: DeviceContext -> Canvas ()
drawGrid context = do -- Draw the lines in the X direction and the Y direction
  drawGridXLines context
  drawGridYLines context

-- We write drawGridXLines to not take an int, then we create a partial application function
-- this makes is to we don't need to call drawGridXLines with 0, since that would be confusing for people reading the code 
drawGridXLines :: DeviceContext -> Canvas ()
drawGridXLines context = drawGridXLine 0 where
  drawGridXLine i = do
    beginPath ()
    moveTo (x, y)
    lineTo (x, y + dy)
    stroke()
    when (i < 10) $ drawGridXLine (i + 1)
    where
        x = gridXPadding context + fromIntegral i * gridSize context + fromIntegral i * gridLineWidth
        y = gridYPadding context
        dy = fromIntegral matrixVisibleHeight * gridSize context + fromIntegral matrixVisibleHeight * gridLineWidth -- Account for both the grid squares you were drawing and also the lines in between

drawGridYLines :: DeviceContext -> Canvas ()
drawGridYLines context = drawGridYLine 0 where
  drawGridYLine i = do
    beginPath ()
    moveTo (x, y)
    lineTo (x + dx, y)
    stroke()
    when (i < 20) $ drawGridYLine (i + 1) -- Draw the next line as long as i is less than 20
    where
      x = gridXPadding context
      dx = fromIntegral matrixWidth * gridSize context + fromIntegral matrixWidth * gridLineWidth -- Same deal here 
      y = gridYPadding context + fromIntegral i * gridSize context + fromIntegral i * gridLineWidth


gridXPadding :: DeviceContext -> Double
-- Get get the padding by subtracting the space the grid takes up from the width
-- We add 1 for the line width since we need an extra line to close off the grid
gridXPadding context = (width context - gridSize context * fromIntegral matrixWidth - gridLineWidth * (fromIntegral matrixWidth + 1)) / 2

gridYPadding :: DeviceContext -> Double 
-- Get get the padding by subtracting the space the grid takes up from the height
-- We add 1 for the line width since we need an extra line to close off the grid
gridYPadding context = (height context - gridSize context * fromIntegral matrixVisibleHeight - gridLineWidth * (fromIntegral matrixVisibleHeight + 1)) / 2

-- Get the size of the grid by calculating the highest size we can have with at least 60 padding in the y axis
gridSize :: DeviceContext -> Double 
gridSize context = trySize 1 where
  trySize i
    | yLeft <= minYPadding = i - 1
    | otherwise = trySize (i + 1)
    where
      yLeft = height context - (gridLineWidth * 21 + i * 20)

minYPadding :: Double 
minYPadding = 50

gridLineWidth :: Double 
gridLineWidth =  1