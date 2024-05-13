-- Import necessary modules
import Graphics.Blank
import Control.Monad
import Data.Text

-- Define or import DeviceContext
data DeviceContext = DeviceContext { width :: Double, height :: Double }

-- Define or import Matrix and MatrixController
data Matrix = Matrix [[Cell]]
data Cell = Cell { state :: CellState, pieceType :: PieceType }
data CellState = Empty | Shadow | Occupied
data PieceType = Line | Square | T | OrangeL | BlueL | RedZ | GreenZ

-- Define or import necessary functions

-- DrawGrid module implementation

drawBackground :: DeviceContext -> Canvas ()
drawBackground context = do
  fillStyle "#000000" -- Set background color to black
  fillRect (0, 0, width context, height context) -- Fill the entire canvas with black

  -- Draw grid lines
  strokeStyle "#333333" -- Set grid line color to dark gray
  lineWidth gridLineWidth -- Set grid line width

  -- Draw horizontal grid lines
  forM_ [0 .. matrixVisibleHeight] $ \i -> do
    let y = gridYPadding context + fromIntegral i * (gridSize context + gridLineWidth)
    beginPath ()
    moveTo (gridXPadding context, y)
    lineTo (gridXPadding context + fromIntegral matrixWidth * (gridSize context + gridLineWidth), y)
    stroke ()

  -- Draw vertical grid lines
  forM_ [0 .. matrixWidth] $ \j -> do
    let x = gridXPadding context + fromIntegral j * (gridSize context + gridLineWidth)
    beginPath ()
    moveTo (x, gridYPadding context)
    lineTo (x, gridYPadding context + fromIntegral matrixVisibleHeight * (gridSize context + gridLineWidth))
    stroke ()
