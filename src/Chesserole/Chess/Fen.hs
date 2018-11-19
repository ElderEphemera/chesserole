{-# LANGUAGE RecordWildCards #-}

-- See <https://ghc.haskell.org/trac/ghc/ticket/14630>
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chesserole.Chess.Fen where

import Data.Char (toLower)
import Data.List (intercalate)

import Chesserole.Chess.Game

--------------------------------------------------------------------------------

fen :: Game -> String -- TODO: Text?
fen Game{..} = unwords
  [ fenBoard gameBoard
  , fenPlayer gamePlayer
  , fenCastle gameCastling
  , fenEnPassant gameEnPassant
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

fenEnPassant :: Maybe Square -> String
fenEnPassant Nothing  = "-"
fenEnPassant (Just s) = anSquare s

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
