{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Chesserole.Chess.Game where

import Control.Applicative  (ZipList(..))
import Data.Functor.Compose (Compose(..))
import Linear               (V2(..))
import Linear.Affine        (Point(..))

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

newtype Board' a = Board { boardPieces :: [[a]] }
  deriving (Eq, Ord, Show)
  deriving (Functor, Applicative) via (Compose ZipList ZipList)

type Board = Board' (Maybe Piece)

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
  } deriving (Eq, Show)

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
initialBoard = Board $
  [ buildRank Black backRank, buildRank Black pawnRank
  , emptyRank, emptyRank, emptyRank, emptyRank
  , buildRank White pawnRank, buildRank White backRank
  ] where
      emptyRank = replicate 8 Nothing
      buildRank color = map (Just . Piece color)
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

getAtSquare :: Square -> Board' a -> a
getAtSquare (Square file rank) (Board pieces) = pieces !! rank !! file

setAtSquare :: Square -> Maybe Piece -> Board -> Board
setAtSquare (Square file rank) maypiece (Board pieces) =
  Board $ modifyInList rank (setInList file maypiece) pieces

forceMovePiece :: Square -> Square -> Board -> Board
forceMovePiece from to board =
  let maypiece = getAtSquare from board
  in setAtSquare to maypiece $ setAtSquare from Nothing board

-- TODO: Special actions: en passant, castling, promotion?
movePiece :: Square -> Square -> Game -> Game
movePiece from to Game{..} = Game
  { gameBoard = forceMovePiece from to gameBoard
  , gamePlayer = invColor gamePlayer
  , gameCastling = gameCastling -- TODO
  , gameEnPassant = gameEnPassant -- TODO
  , gameClock = gameClock -- TODO
  , gameMoves = gameMoves + fromEnum gamePlayer
  }

--------------------------------------------------------------------------------

-- TODO: Probably shoudn't use @[]@ for 'Board'
modifyInList :: Int -> (a -> a) -> [a] -> [a]
modifyInList 0 f (x:xs) = f x : xs
modifyInList n f (x:xs) = x : modifyInList (n-1) f xs
modifyInList _ _ [] = error "modifyinlist: invalid index"

setInList :: Int -> a -> [a] -> [a]
setInList n = modifyInList n . const

--------------------------------------------------------------------------------

data Move
  = Move Square Square
  | Castle CastleSide
  deriving (Eq, Ord, Show)
