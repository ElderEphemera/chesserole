{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- PatternSynonyms <> Recordwildcards = bug
-- TODO: Reference the Trac ticket here
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chesserole.Chess where

import Data.Char     (toLower)
import Data.List     (intercalate)
import Linear        (V2(..))
import Linear.Affine (Point(..))

-- TODO: Organize

--------------------------------------------------------------------------------

data Color = White | Black
  deriving (Eq, Ord, Show, Enum)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show, Enum)

data Piece = Piece
  { pieceColor :: Color
  , pieceType :: PieceType
  } deriving (Eq, Ord, Show)

newtype Board = Board { boardPieces :: [[Maybe Piece]] }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- TODO: Square: 'Point' newtype or product type?
newtype Square = Square' { squarePos :: Point V2 Int }
  deriving (Eq, Show)

-- TODO: (rank, file) or (file, rank)?
{-# COMPLETE Square #-}
pattern Square :: Int -> Int -> Square
pattern Square{squareRank, squareFile} <- Square' (P (V2 squareRank squareFile))

square :: Int -> Int -> Maybe Square
square rank file
  | inBounds rank && inBounds file = Just (Square' (P (V2 rank file)))
  | otherwise = Nothing

squareP :: Point V2 Int -> Maybe Square
squareP vec@(P (V2 rank file))
  | inBounds rank && inBounds file = Just (Square' vec)
  | otherwise = Nothing

inBounds :: Int -> Bool
inBounds n = 0 <= n && n < 8

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
  , gamePassant :: Maybe Square
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

getPiece :: Square -> Board -> Maybe Piece
getPiece (Square rank file) (Board pieces) = pieces !! file !! rank

setPiece :: Square -> Maybe Piece -> Board -> Board
setPiece (Square rank file) maypiece (Board pieces) =
  Board $ modifyInList file (setInList rank maypiece) pieces

forceMovePiece :: Square -> Square -> Board -> Board
forceMovePiece from to board =
  let maypiece = getPiece from board
  in setPiece from Nothing $ setPiece to maypiece board

--------------------------------------------------------------------------------

-- TODO: Probably shoudn't use @[]@ for 'Board'
modifyInList :: Int -> (a -> a) -> [a] -> [a]
modifyInList 0 f (x:xs) = f x : xs
modifyInList n f (x:xs) = x : modifyInList (n-1) f xs
modifyInList _ _ [] = error "modifyinlist: invalid index"

setInList :: Int -> a -> [a] -> [a]
setInList n = modifyInList n . const

--------------------------------------------------------------------------------

fen :: Game -> String -- TODO: Text?
fen Game{..} = unwords
  [ fenBoard gameBoard
  , fenPlayer gamePlayer
  , fenCastle gameCastling
  , fenPassant gamePassant
  , show gameClock
  , show gameMoves
  ]

fenBoard :: Board -> String
fenBoard = intercalate "/" . fmap fenRank . boardPieces

fenRank :: [Maybe Piece] -> String
fenRank = go 0
  where go n []             = fenEmpty n
        go n (Nothing : ps) = go (n+1) ps
        go n (Just p  : ps) = fenEmpty n <> fenPiece p <> go 0 ps

fenPiece :: Piece -> String
fenPiece Piece{..} = fenColorCase pieceColor $ fenPieceType pieceType

fenColorCase :: Color -> String -> String
fenColorCase White = id
fenColorCase Black = map toLower

fenPieceType :: PieceType -> String
fenPieceType Pawn   = "P"
fenPieceType Knight = "N"
fenPieceType Bishop = "B"
fenPieceType Rook   = "R"
fenPieceType Queen  = "Q"
fenPieceType King   = "K"

fenEmpty :: Int -> String
fenEmpty 0 = ""
fenEmpty n = show n

fenPlayer :: Color -> String
fenPlayer White = "w"
fenPlayer Black = "b"

fenCastle :: [CastleType] -> String
fenCastle []  = "-"
fenCastle cts = fenCastle1 =<< cts

fenCastle1 :: CastleType -> String
fenCastle1 CastleType{..} = fenColorCase castleColor $ fenCastleSide castleSide

fenCastleSide :: CastleSide -> String
fenCastleSide QueenSide = "Q"
fenCastleSide KingSide  = "K"

fenPassant :: Maybe Square -> String
fenPassant Nothing  = "-"
fenPassant (Just s) = anSquare s

--------------------------------------------------------------------------------

data Move
  = Move Square Square
  | Castle CastleSide

--------------------------------------------------------------------------------

anMove :: Move -> String
anMove (Move from to) = anSquare from <> anSquare to
anMove (Castle QueenSide) = "O-O-O"
anMove (Castle KingSide) = "O-O"

anSquare :: Square -> String
anSquare Square{..} = anFile squareFile <> anRank squareRank

anRank :: Int -> String
anRank = show

-- TODO: hacky
anFile :: Int -> String
anFile = pure . toEnum . (fromEnum 'a' +)

parseAnMove :: String -> Maybe Move
parseAnMove "O-O-O" = Just (Castle QueenSide)
parseAnMove "O-O" = Just (Castle KingSide)
parseAnMove [fromFile, fromRank, toFile, toRank] = Move
  <$> parseAnSquare [fromFile, fromRank]
  <*> parseAnSquare [toFile, toRank]
parseAnMove _ = Nothing

-- TODO: hacky
parseAnSquare :: String -> Maybe Square
parseAnSquare [file, rank] = square (dist '1' rank) (dist 'a' file)
  where dist x y = fromEnum y - fromEnum x
parseAnSquare _ = Nothing
