---- En este modulo se incluyen aquellas funciones que no requieren del estado del juegos
module Prototipo where

type Cell = Maybe Int
type Board2 = [[Cell]]
type Position2 = (Int, Int)
type Piece = [Position2]

boardHeight :: Int
boardHeight = 20

-- POST: Asocia a cada pieza un número
pieceType :: Piece -> Int
pieceType piece = case piece of
    _ | piece == iPiece -> 1
    _ | piece == oPiece -> 2
    _ | piece == tPiece -> 3
    _ | piece == sPiece -> 4
    _ | piece == zPiece -> 5
    _ | piece == jPiece -> 6
    _ | piece == lPiece -> 7
    _ -> error "Invalid piece"


-- POST: Pieza larga, perfecta para los bordes del juego, con sus respectivos centros de gravedad
iPiece :: Piece
iPiece = [(0, 0), (0, 1), (0, 2), (0, 3)]

-- POST: Pieza cuadrada con sus respectivos centros de gravedad
--Si, si quieres girar el cuadrado puedes :)
oPiece :: Piece
oPiece = [(0, 0), (0, 1), (1, 0), (1, 1)]

-- POST: Pieza T-spin con sus respectivos centros de gravedad
tPiece :: Piece
tPiece = [(-1, 0), (0, 0), (1, 0), (0, 1)]

-- POST: Pieza estilo S, odiada por muchos y querida por pocos, con sus respectivos centros de gravedad
sPiece :: Piece
sPiece = [(-1, 0), (0, 0), (0, 1), (1, 1)]

-- POST: Pieza estilos S, hermana de la S, con sus respectivos centros de gravedad
zPiece :: Piece
zPiece = [(1, 0), (0, 0), (0, 1), (-1, 1)]

-- POST: Pieza estilos J, casi casi la larga, con sus respectivos centros de gravedad
jPiece :: Piece
jPiece = [(-1, 0), (0, 0), (1, 0), (1, 1)]

-- POST: Pieza estilos L, casi casi la larga, con sus respectivos centros de gravedad
lPiece :: Piece
lPiece = [(-1, 0), (0, 0), (1, 0), (-1, 1)]

-- POST: inicializa el tablero vacio
emptyBoard :: Int -> Int -> Board2
emptyBoard width height = replicate height (replicate width Nothing)

numberToPiece :: Int -> Piece
numberToPiece n = case n of
    1 -> iPiece
    2 -> oPiece
    3 -> tPiece
    4 -> sPiece
    5 -> zPiece
    6 -> jPiece
    7 -> lPiece
    _ -> error "Invalid piece number"

isNothing :: Cell -> Bool
isNothing Nothing = True
isNothing _ = False

getCell :: Board2 -> Position2 -> Cell
getCell board (x, y) = (board !! y) !! x

setCell :: Board2 -> Position2 -> Cell -> Board2
setCell board (x, y) cell = 
    let (top, row:bottom) = splitAt y board
        (left, _:right) = splitAt x row
    in top ++ [left ++ [cell] ++ right] ++ bottom

-- Given a gameState, it will get the actual piece and look for the max Y increase value, by choosing the
-- maximum Y value of the piece (map snd piece, gets only the seconds position of the pieces, hence the y values)
-- If it doesnt touch the bottom, it will remove the actual piece, so that it cannot collide with itself 
-- and then check if it collide with any other piece below.
noContactBelowShadow :: Board2 -> Piece -> Position2 -> Bool
noContactBelowShadow b piece pos =
    let (x, y) = pos
        maxY = maximum $ map snd piece
    in ( y + maxY + 1 < boardHeight )
        &&
        all (\(dx, dy) -> case getCell b (x + dx, y + dy + 1) of
            Just 69 -> True
            Just _ -> False
            Nothing -> True) piece

-- Moves the piece down until it collides with sth, then movePieceDown will change the piece
instantDropShadow :: Board2 -> Piece -> Position2 -> Board2
instantDropShadow b piece pos =
    let (x, y) = pos
    in  if noContactBelowShadow b piece pos
        then instantDropShadow b piece (x, y + 1)
        else printShadow b piece pos

printShadow :: Board2 -> Piece -> Position2 -> Board2
printShadow b piece pos =
    let (x, y) = pos
    in foldl (\b (dx, dy) -> setCell b (x + dx, y + dy) (Just 69)) b piece

isJust :: Cell -> Bool
isJust Nothing = False
isJust (Just 69) = False
isJust (Just _) = True