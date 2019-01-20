{-# LANGUAGE RecordWildCards #-}

module Chesserole.Chess.Game where

import qualified Data.Map.Strict as M
import Linear        (V2(..))
import Linear.Affine (Point(..))

--------------------------------------------------------------------------------

data Color = White | Black
  deriving (Eq, Ord, Show, Enum)

invColor :: Color -> Color
invColor White = Black
invColor Black = White

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show, Enum)

data Piece = Piece
  { pieceColor :: Color
  , pieceType :: PieceType
  } deriving (Eq, Ord, Show)

type Board = M.Map Square Piece

--------------------------------------------------------------------------------

data Square = Square { squareFile, squareRank :: Int }
  deriving (Eq, Ord, Show)

square :: Int -> Int -> Maybe Square
square file rank
  | inBounds file && inBounds rank = Just (Square file rank)
  | otherwise = Nothing
  where inBounds n = 0 <= n && n < 8

pointSquare :: Point V2 Int -> Square
pointSquare (P (V2 file rank)) = Square file rank

shiftSquare :: Square -> V2 Int -> Maybe Square
shiftSquare (Square file rank) (V2 x y) = square (file + x) (rank + y)

--------------------------------------------------------------------------------

data CastleSide = QueenSide | KingSide
  deriving (Eq, Ord, Show, Enum)

data CastleType = CastleType
  { castleColor :: Color
  , castleSide :: CastleSide
  } deriving (Eq, Ord, Show)

data Game = Game
  { gameBoard :: Board
  , gamePlayer :: Color
  , gameCastling :: [CastleType] -- TODO: Set?
  , gameEnPassant :: Maybe Square
  , gameClock :: Int
  , gameMoves :: Int
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

initialBoard :: Board
initialBoard = M.unions $
  [ buildRank 0 Black backRank
  , buildRank 1 Black pawnRank
  , buildRank 6 White pawnRank
  , buildRank 7 White backRank
  ] where
      buildRank squareRank color
        = M.mapKeys (\squareFile -> Square{..})
        . fmap (Piece color)
        . M.fromAscList . zip [0..7]
      pawnRank = replicate 8 Pawn
      backRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

initialGame :: Game
initialGame = Game
  { gameBoard = initialBoard
  , gamePlayer = White
  , gameCastling = CastleType <$> [White, Black] <*> [QueenSide, KingSide]
  , gameEnPassant = Nothing
  , gameClock = 0
  , gameMoves = 1
  }

--------------------------------------------------------------------------------

getAtSquare :: Square -> Board -> Maybe Piece
getAtSquare = M.lookup

setAtSquare :: Square -> Maybe Piece -> Board -> Board
setAtSquare sq maypiece = M.alter (const maypiece) sq

forceMovePiece :: Square -> Square -> Board -> Board
forceMovePiece from to board =
  let maypiece = getAtSquare from board
  in setAtSquare to maypiece $ setAtSquare from Nothing board
