{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module Chesserole.Chess.Game where

import Control.Applicative  (ZipList(..))
import Data.Functor.Compose (Compose(..))
import Linear               (V2(..))
import Linear.Affine        ((.+^), Point(..))

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

-- TODO: @Point V2 IntMod8@?
newtype Square = Square' { squarePos :: Point V2 Int }
  deriving (Eq, Ord, Show)

{-# COMPLETE Square #-}
pattern Square :: Int -> Int -> Square
pattern Square{squareFile, squareRank} <- Square' (P (V2 squareFile squareRank))

square :: Int -> Int -> Maybe Square
square file rank
  | inBounds file && inBounds rank = Just (Square' (P (V2 file rank)))
  | otherwise = Nothing

squareP :: Point V2 Int -> Maybe Square
squareP pt@(P (V2 file rank))
  | inBounds file && inBounds rank = Just (Square' pt)
  | otherwise = Nothing

squareModP :: Point V2 Int -> Square
squareModP = Square' . fmap (`rem` 8)

inBounds :: Int -> Bool
inBounds n = 0 <= n && n < 8

shiftSquare :: Square -> V2 Int -> Maybe Square
shiftSquare (Square' pt) v = squareP $ pt .+^ v

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
  , gameMoves = 0
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
