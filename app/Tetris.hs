{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Tetris where
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (partition, delete)
import System.Random 
import Control.Monad (forM)


import Prototipo (Cell, Board2, Position2, Piece, pieceType, iPiece, oPiece, tPiece, sPiece, zPiece, jPiece, lPiece
   , emptyBoard, numberToPiece, isNothing, setCell, getCell, noContactBelowShadow, instantDropShadow, printShadow, isJust)

-- import Text.Printf (IsChar(toChar))
-- import Control.Monad
-- import Data.IORef 
-- import Data.Text
-- import Data.Char (ord)

data Position = Position Double Double deriving(Eq)
data Block = Empty | PiezaI | PiezaO | PiezaT | PiezaS | PiezaZ | PiezaJ | PiezaL | Shadow deriving(Eq)
data Fila = Fila Double deriving(Eq)
data Columna = Columna Double deriving(Eq)
type Board = [[Block]]

data GameState = GameState
    { board :: Board2
    , listPieces :: [Int]
    , nextPieceIndex :: Int
    , currentPiece :: (Piece, Position2)
    , currentPieceType :: Int
    , score :: Int
    , level :: Int
    , gameEnded :: Bool
    }

-- Variables de tamaños
blockSize :: Double
blockSize = 30
boardWidth :: Int
boardWidth = 10
boardHeight :: Int
boardHeight = 20

--POST: Crea una ventana con el juego concurrente del tetris

tetrisMain :: Int -> IO ()
tetrisMain startingLevel = do
    _ <- initGUI
    -- Crea una nueva ventana
    window <- windowNew
    drawingTetris <- drawingAreaNew
    set window [ windowTitle := "Tetris juego"]  
    windowMaximize window
    widgetModifyBg window StateNormal (Color (63 * 257) (36 * 257) (99 * 257))

    
    let startingBoard = drawPieceOnBoard (initializeGameState startingLevel)
    let startedGameState = drawShadow (initializeGameState startingLevel)
    let myBoard = board2ToBoard (board startedGameState)
    _ <- on drawingTetris exposeEvent $ tryEvent $ liftIO $ drawWindow drawingTetris myBoard
    scoreText <- labelNew (Just ("NIVEL: \t\tPUNTOS: \n\n " ++ show startingLevel ++ " \t\t" ++ show 0))
    ultimateGameState <- bucleJuego window drawingTetris (startingGameState startingLevel startingBoard) scoreText

    -- Texto de controles
    controlsText <- labelNew (Just "\tCONTROLES: \n\n ←  Mover pieza a la izquierda \n →  Mover pieza a la derecha\n ↑  Rotar pieza\n z Rotación derecha \n x Rotación izquierda\n ↓  Bajada corta\nESP Bajada inmediata")
    fontcontrolsText <- fontDescriptionFromString "Monospace 15"
    widgetModifyFont controlsText (Just fontcontrolsText)
    widgetModifyFg controlsText StateNormal (Color (255 * 257) (255 * 257) (255 * 257))
    --Texto de puntuación
    fontscoreText <- fontDescriptionFromString "Monospace 15"
    widgetModifyFont scoreText (Just fontscoreText)
    widgetModifyFg scoreText StateNormal (Color (255 * 257) (255 * 257) (255 * 257))

        
    -- Crea un Box para centrar objetos 
    box <- hBoxNew True 0
    -- Establece el tamaño de los textos 
    boxPackStart box controlsText PackNatural 0
    boxPackStart box drawingTetris PackGrow 0
    boxPackStart box scoreText PackNatural 0
    -- Añade el Box a la ventana
    containerAdd window box 


    -- Muestra la ventana
    widgetShowAll window
    -- Cierra la aplicación cuando se cierra la ventana
    _ <- on window objectDestroy mainQuit
    mainGUI

  

---- TABLERO

-- POST: Crea la tabla, una lista de listas
mainBoard :: Board
mainBoard = replicate boardHeight (replicate boardWidth Empty)

-- POST: Se establece una pieza en una posición de un tablero
drawBoard :: Position -> Board -> Render ()
drawBoard (Position offsetX offsetY) board = do
    -- Aplica un desplazamiento al tablero para centrarlo
    mapM_ (drawRowWithIndex (Position offsetX offsetY)) $ zip board [0..]

-- POST: Dibuja en una fila especificada del tablero teniendo en cuenta donde hay bloques
drawRowWithIndex :: Position -> ([Block], Int) -> Render ()
drawRowWithIndex (Position offsetX offsetY) (row, y) =
    mapM_ (\(block, x) -> drawBlock (Position (offsetX + fromIntegral x * blockSize) (offsetY + fromIntegral y * blockSize)) block) $ zip row [0..]

-- POST: Se crea a partir de una posición y un tablero la cuadricula del juego con la posición especificada
drawGrid :: Position -> Board -> Render ()
drawGrid (Position offsetX offsetY) board = do
    -- Personalización de la cuadricula
    setSourceRGB 0.0 0.0 0.0  -- Color negro
    setLineWidth 1.0
    let width = fromIntegral (length (head board)) * blockSize
        height = fromIntegral (length board) * blockSize
    mapM_ (\x -> moveTo (offsetX + x) offsetY >> lineTo (offsetX + x) (offsetY + height) >> stroke) [0, blockSize .. width]
    mapM_ (\y -> moveTo offsetX (offsetY + y) >> lineTo (offsetX + width) (offsetY + y) >> stroke) [0, blockSize .. height]

-- POST: Renderiza el tablero centradolo en la ventana
drawWindow :: DrawingArea -> Board -> IO ()
drawWindow da board = do
    draw <- widgetGetDrawWindow da
    (winWidth, winHeight) <- widgetGetSize da
    let boardWidthPx = fromIntegral (length (head board)) * blockSize
        boardHeightPx = fromIntegral (length board) * blockSize
        offsetX = (fromIntegral winWidth - boardWidthPx) / 2
        offsetY = (fromIntegral winHeight - boardHeightPx) / 2
    renderWithDrawable draw $ do
        drawBoard (Position offsetX offsetY) board
        drawGrid (Position offsetX offsetY) board

-- POST: Dibuja un bloque en una posición especificada
drawBlock :: Position -> Block -> Render ()
drawBlock (Position x y)  block =
    case block of
        PiezaI -> do
            setSourceRGB 0.0 1.0 1.0  -- Color cian
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        PiezaO -> do
            setSourceRGB 1.0 1.0 0.0  -- Color amarillo
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        PiezaT -> do
            setSourceRGB 0.7 0.0 0.5  -- Color morado
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        PiezaS -> do
            setSourceRGB 0.0 1.0 0.0  -- Color verde
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        PiezaZ -> do
            setSourceRGB 1.0 0.0 0.0  -- Color rojo
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        PiezaJ -> do
            setSourceRGB 0.0 0.0 1.0  -- Color azul
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        PiezaL -> do
            setSourceRGB 1.0 0.5 0.0  -- Color naranja
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        Empty -> do
            setSourceRGB 1.0 1.0 1.0  -- Color blanco
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill
        Shadow -> do
            setSourceRGB 0.5 0.5 0.5  -- Color gris
            Graphics.Rendering.Cairo.rectangle x y blockSize blockSize
            fill


---- TRADUCCION (PROTOTIPO -> INTERFAZ)

-- POST: Relaciona celdas con bloques, se pasa de lo lógico a lo gráfico 
cellToBlock :: Cell -> Block
cellToBlock Nothing = Empty
cellToBlock (Just 69) = Shadow
cellToBlock (Just 1) = PiezaI
cellToBlock (Just 2) = PiezaO
cellToBlock (Just 3) = PiezaT
cellToBlock (Just 4) = PiezaS
cellToBlock (Just 5) = PiezaZ
cellToBlock (Just 6) = PiezaJ
cellToBlock (Just 7) = PiezaL
-- cellToBlock (Just _) = Filled

-- POST: Dadado un tablero de celdas, se pasa a un tablero de bloques
board2ToBoard :: Board2 -> Board
board2ToBoard = map (map cellToBlock)


---- JUEGO

-- POST: Se inicializa el juego con un tablero vacío
startingGameState :: Int -> Board2 ->  GameState
startingGameState nivel startingBoard = (initializeGameState nivel) { board = startingBoard }

-- POST: Marca el punto de inicio de la pieza
startingPoint :: Position2
startingPoint = (5, 0)


-- POST: Bucle del juego, se encarga de gestionar los eventos del teclado, es decir, los controles
bucleJuego :: Window -> DrawingArea -> GameState -> Label -> IO GameState
bucleJuego windowTetris da gs scoreText = do
    gsRef <- newIORef gs
    _ <- on windowTetris keyPressEvent $ tryEvent $ do
        keyName <- eventKeyName
        let x = show keyName
        case x of 
            "\"Right\"" -> do
                liftIO $ modifyIORef gsRef movePieceRight
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS)) 
            "\"Left\"" -> do 
                liftIO $ modifyIORef gsRef movePieceLeft
                -- shadowGS <- drawShadow gs
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS)) 
            "\"Up\"" -> do
                liftIO $ modifyIORef gsRef rotatePieceClockWise
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS)) 
            "\"Down\"" -> do
                liftIO $ modifyIORef gsRef movePieceDown
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS)) 
            "\"space\"" -> do
                liftIO $ modifyIORef gsRef instantDrop
                --shadowGS <- drawShadow gs
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS)) 
            "\"z\"" -> do
                liftIO $ modifyIORef gsRef rotatePieceCounterClockWise
                --shadowGS <- drawShadow gs
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS))
            "\"x\"" -> do
                liftIO $ modifyIORef gsRef rotatePieceClockWise
                --shadowGS <- drawShadow gs
                newGS <- liftIO $ readIORef gsRef
                let shadowGS = drawShadow newGS
                liftIO $ drawWindow da (board2ToBoard (board shadowGS))
            _ -> liftIO $ putStrLn $ "Key pressed: " ++ show keyName -- esto habria que borrarlo
    newGS <- readIORef gsRef
    _ <- timeoutAdd (do
        liftIO $ modifyIORef gsRef movePieceDown
        newGS <- readIORef gsRef
        let shadowGS = drawShadow newGS
            newScore = score newGS
            newLevel = level newGS
            newText = "NIVEL: \t\tPUNTOS: \n\n " ++ show newLevel ++ " \t\t" ++ show newScore
        labelSetText scoreText newText
        drawWindow da (board2ToBoard (board shadowGS))
        if gameEnded shadowGS
        then do
            widgetDestroy windowTetris
            gameOver
            return False
        else
            return True -- Return True to keep the timer running
        ) (round (((0.8 - (fromIntegral(level newGS - 1) * 0.007)) ** fromIntegral(level newGS - 1))*1000)) -- 1000 milliseconds = 1 second
    return newGS

-- POST: Se encarga crea una ventana al finalizar el juego (TODO: usarlo cuando sea necesario)
gameOver :: IO ()
gameOver = do 
    -- Inicializa GTK
    _ <- initGUI
    -- Crea una nueva ventana
    window <- windowNew
    set window [ windowTitle := "Tetris"] 
    windowMaximize window
    widgetModifyBg window StateNormal (Color (63 * 257) (36 * 257) (99 * 257))

    -- Crea un botón
    buttonPlay <- buttonNewWithLabel "VOLVER A JUGAR"
    widgetSetSizeRequest buttonPlay 150 150
    _ <- onClicked buttonPlay $ do
        widgetDestroy window
        tetrisMain 1
    -- Obtiene el Label del botón y cambia el tamaño de la fuente
    maybeLabel <- binGetChild buttonPlay
    case maybeLabel of
        Nothing -> return ()
        Just label -> do
            let labelWidget = castToLabel label
            fontButton <- fontDescriptionFromString "Monospace 30"
            widgetModifyFont labelWidget (Just fontButton)
    -- Crear titulos y subtitulo   
    title <- labelNew (Just "GAME OVER!!")
    fontTitle <- fontDescriptionFromString "Monospace 50"
    widgetModifyFont title (Just fontTitle)
    widgetModifyFg title StateNormal (Color (255 * 257) (255 * 257) (255 * 257))

    -- Crea un Box para centrar el botón y el título
    box <- vBoxNew False 0
    -- Establece el tamaño de subtitle
    boxPackStart box title PackNatural 100
    boxPackStart box buttonPlay PackNatural 50
    -- Añade el Box a la ventana
    containerAdd window box 

    -- Muestra la ventana
    widgetShowAll window

    -- Cierra la aplicación cuando se cierra la ventana
    _ <- on window objectDestroy mainQuit

    -- Comienza el bucle principal de GTK
    mainGUI    

---- Cambio de estado del juego
-- changeCurrentPiece, changeBoard, initializeGameState, refreshGameState, drawPieceOnBoard, movePiece, movePieceDown, noContactBelow, instantDrop, 
-- movePieceLeft, noContactLeft, movePieceRight, noContactRight, rotatePieceClockWise, rotatePiece, rotatePieceCounterClockWise, noContactRotation, 
-- drawShadow, comprobarLineas

changeCurrentPiece :: GameState -> Piece -> Position2 -> GameState
changeCurrentPiece gameState newPiece newPosition = gameState { currentPiece = (newPiece, newPosition) }

changeBoard :: GameState -> Board2 -> GameState
changeBoard gameState newBoard = gameState { board = newBoard }

initializeGameState :: Int -> GameState
initializeGameState startingLevel = 
    let initialBoard = emptyBoard 10 20
        initialListPieces = generatePieceOrder 1
        initialCurrentPiece = (numberToPiece (head initialListPieces), startingPoint)
        initialTypePiece = pieceType (fst initialCurrentPiece)
    in GameState
        { board = initialBoard
        , listPieces = initialListPieces
        , nextPieceIndex = 1
        , currentPiece = initialCurrentPiece
        , currentPieceType = initialTypePiece
        , score = 0
        , level = startingLevel
        , gameEnded = False
        }


refreshGameState :: GameState -> GameState
refreshGameState gameState = 
    let clearedGameState = comprobarLineas gameState
        (newPieceList, newNextPieceIndex) =
            if (nextPieceIndex clearedGameState) == 6
            then (generatePieceOrder newScore, 0)
            else ((listPieces clearedGameState), (nextPieceIndex clearedGameState) + 1)
        newCurrentPiece = (numberToPiece ((listPieces clearedGameState) !! (nextPieceIndex clearedGameState)), startingPoint)
        newTypePiece = pieceType (fst newCurrentPiece)
        auxGameState = clearedGameState {currentPiece = newCurrentPiece}
        newGameEnded = let (x,y) = snd (currentPiece gameState)
                       in not (noContactBelow auxGameState) && any (\piece -> (snd piece) + y == 0) (fst newCurrentPiece)
        newBoard = drawPieceOnBoard auxGameState
        newScore = score clearedGameState
        previousLevel = level clearedGameState
        newLevel = previousLevel + (div newScore (1000*previousLevel))
    in gameState {currentPiece = newCurrentPiece, currentPieceType = newTypePiece, nextPieceIndex = newNextPieceIndex, board = newBoard, listPieces = newPieceList, level = newLevel, score = newScore, gameEnded = newGameEnded}

drawPieceOnBoard :: GameState -> Board2
drawPieceOnBoard gameState = 
    let (piece, (x, y)) = currentPiece gameState
        currentBoard = board gameState
        actualPieceType = currentPieceType gameState
    in foldl (\b (dx, dy) -> if isNothing (getCell b (x + dx, y + dy)) 
                             then setCell b (x + dx, y + dy) (Just actualPieceType) 
                             else b) currentBoard piece

-- Given a movement, and a gameState, it moves the piece in the direction of the movement
-- It REMOVEs the piece from the current position and adds it to the new position
-- Comun in all movements functions
movePiece :: (Int, Int) -> GameState -> GameState
movePiece (dx, dy) gameState =
    let (piece, (x, y)) = currentPiece gameState
        typePiece = currentPieceType gameState
        newBoard = foldl (\b (dx', dy') -> setCell b (x + dx', y + dy') Nothing) (board gameState) piece -- reset current cells to False
        newBoard' = foldl (\b (dx', dy') -> setCell b (x + dx' + dx, y + dy' + dy) (Just typePiece)) newBoard piece -- assign new cells to True
    in gameState { currentPiece = (piece, (x + dx, y + dy)), board = newBoard' }

-- Given a gameState
-- It then looks if the piece go out of the board or if it collides with another piece below.
    -- If doesn't collide, it moves the piece down with the function movePiece
    -- If it collides, it will refresh the game state with the function refreshGameState
movePieceDown :: GameState -> GameState 
movePieceDown gameState 
    | noContactBelow gameState =    let movedState = movePiece (0, 1) gameState
                                    in movedState {score = score gameState + 1}
    | otherwise = refreshGameState gameState

-- Given a gameState, it will get the actual piece and look for the max Y increase value, by choosing the
-- maximum Y value of the piece (map snd piece, gets only the seconds position of the pieces, hence the y values)
-- If it doesnt touch the bottom, it will remove the actual piece, so that it cannot collide with itself 
-- and then check if it collide with any other piece below.
noContactBelow :: GameState -> Bool
noContactBelow gameState =
    let (piece, (x, y)) = currentPiece gameState
        maxY = maximum $ map snd piece
    in ( y + maxY + 1 < boardHeight )
        &&
        let boardWithoutCurrentPiece = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) Nothing) (board gameState) piece
        in  all (\(dx, dy) -> case getCell boardWithoutCurrentPiece (x + dx, y + dy + 1) of
                Just 69 -> True
                Just _ -> False
                Nothing -> True) piece

-- Moves the piece down until it collides with sth, then movePieceDown will change the piece
instantDrop :: GameState -> GameState
instantDrop gameState =
    if noContactBelow gameState
    then    let movedState = movePieceDown gameState
                movedStateWithScore = movedState {score = score movedState + 1}
            in instantDrop movedStateWithScore
    else movePieceDown gameState

movePieceLeft :: GameState -> GameState
movePieceLeft gameState =
    if noContactLeft gameState  -- check if the piece can move left without going out of the board
    then movePiece (-1, 0) gameState  -- move the piece to the left
    else gameState  -- if the piece can't move left, return the original game state

noContactLeft :: GameState -> Bool
noContactLeft gameState =
    let (piece, (x, y)) = currentPiece gameState
        minX = minimum $ map fst piece
    in ( x + minX - 1 >= 0 )
        &&
        let boardWithoutCurrentPiece = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) Nothing) (board gameState) piece 
        in  all (\(dx, dy) -> case getCell boardWithoutCurrentPiece (x + dx - 1, y + dy) of
                Just 69 -> True
                Just _ -> False
                Nothing -> True) piece

movePieceRight :: GameState -> GameState
movePieceRight gameState =
    if noContactRight gameState -- check if the piece can move right without going out of the board
    then movePiece (1, 0) gameState  -- move the piece to the right
    else gameState  -- if the piece can't move right, return the original game state

noContactRight :: GameState -> Bool
noContactRight gameState =
    let (piece, (x, y)) = currentPiece gameState
        maxX = maximum $ map fst piece
        boardWidth = length (head (board gameState))
    in ( x + maxX + 1 < boardWidth ) 
        &&
        let boardWithoutCurrentPiece = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) Nothing) (board gameState) piece -- make the current piece disappear from the board, so that it doesnt collide with itself
        in all (\(dx, dy) -> case getCell boardWithoutCurrentPiece (x + dx + 1, y + dy) of
            Just 69 -> True
            Just _ -> False
            Nothing -> True) piece

rotatePieceClockWise :: GameState -> GameState
rotatePieceClockWise gameState =
    let (piece, pos@(x, y)) = currentPiece gameState
        rotatedPiece = map (\(dx, dy) -> (-dy, dx)) piece  -- rotate the piece 90 degrees clockwise
    in if noContactRotation gameState rotatedPiece
       then rotatePiece gameState rotatedPiece  -- update the game state with the rotated piece
       else gameState  -- if the rotated piece would be out of bounds, don't rotate it

rotatePiece :: GameState -> Piece -> GameState
rotatePiece gameState rotatedPiece =
    let (piece, (x, y)) = currentPiece gameState
        typePiece = currentPieceType gameState
        newBoard = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) Nothing) (board gameState) (fst (currentPiece gameState)) -- reset current cells to False
        newBoard' = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) (Just typePiece)) newBoard rotatedPiece -- assign new cells to True
    in gameState { currentPiece = (rotatedPiece, (x, y)), board = newBoard' }

rotatePieceCounterClockWise :: GameState -> GameState
rotatePieceCounterClockWise gameState =
    let (piece, pos@(x, y)) = currentPiece gameState
        rotatedPiece = map (\(dx, dy) -> (dy, -dx)) piece  -- rotate the piece 90 degrees counter-clockwise
    in if noContactRotation gameState rotatedPiece
       then rotatePiece gameState rotatedPiece -- update the game state with the rotated piece
       else gameState  -- if the rotated piece would be out of bounds, don't rotate it

noContactRotation :: GameState -> Piece -> Bool
noContactRotation gameState rotatedPiece = 
    let (x, y) = snd (currentPiece gameState)
        newPiece = map (\(dx, dy) -> (x + dx, y + dy)) rotatedPiece
    in (all (\(px, py) -> px >= 0 && px < boardWidth && py >= 0 && py < boardHeight) newPiece) --Dont go out of bounds
        &&
        let boardWithoutCurrentPiece = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) Nothing) (board gameState) (fst (currentPiece gameState))
        in all (\(dx, dy) -> case getCell boardWithoutCurrentPiece (x + dx, y + dy) of
            Just 69 -> True
            Just _ -> False
            Nothing -> True) rotatedPiece

drawShadow :: GameState -> GameState
drawShadow gameState =
    let (piece, (x, y)) = currentPiece gameState
        typePiece = currentPieceType gameState
        boardWithoutCurrentPiece = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) Nothing) (board gameState) piece

        shadowBoard = instantDropShadow boardWithoutCurrentPiece piece (x, y)
    
        boardWithCurrentPiece = foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) (Just typePiece)) shadowBoard piece
    in gameState { board = boardWithCurrentPiece }

comprobarLineas :: GameState -> GameState
comprobarLineas gs = 
    let clearBoard = board gs
        (completedLines, remainingLines) = partition (all isJust) clearBoard
        emptyLines = replicate (length completedLines) (replicate boardWidth Nothing)
        newBoard = emptyLines ++ remainingLines
        baseScore = case length completedLines of
            1 -> 100
            2 -> 300
            3 -> 500
            4 -> 800
            _ -> 0
        newScore = score gs + baseScore
    in gs { board = newBoard, score = newScore }


generatePieceOrder :: Int -> [Int]
generatePieceOrder gen = shuffle (mkStdGen gen) [1..7] 

-- Función para barajar (shuffle) una lista de forma aleatoria usando un generador puro
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle _ [] = []
shuffle gen xs = 
    let (n, newGen) = randomR (0, length xs - 1) gen
        (left, (a:right)) = splitAt n xs
        shuffledRest = shuffle newGen (left ++ right)
    in a : shuffledRest
