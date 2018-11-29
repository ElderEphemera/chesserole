{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Chesserole.Chess.Moves where

import Control.Monad (join, mfilter)
import Data.Foldable (asum)
import Data.Maybe    (isNothing, mapMaybe, maybeToList)
import Linear        (V2(..))

import Chesserole.Chess.Game

--------------------------------------------------------------------------------

data Move
  = Move Square Square
  | Castle CastleSide
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

validMovesFrom :: Game -> Square -> [Square]
validMovesFrom game@Game{..} from
  =  standardMovement gameBoard from
  <> pawnMovement game from
  <> castlingMovement game from

standardMovement :: Board -> Square -> [Square]
standardMovement board from = do
  Piece{..} <- maybeToList $ getAtSquare from board
  move <- pieceMoves pieceType
  let onBoard = mapMaybe (shiftSquare from) move
  takeWhileAnd (blockTest board pieceColor) onBoard

blockTest :: Board -> Color -> Square -> Maybe Bool
blockTest board color from = (color /=) . pieceColor <$> getAtSquare from board

takeWhileAnd :: (a -> Maybe Bool) -> [a] -> [a]
takeWhileAnd f = foldr go []
  where
    go x r = case f x of
      Nothing    -> x : r
      Just True  -> [x]
      Just False -> []

pieceMoves :: PieceType -> [[V2 Int]]
pieceMoves = \case
  Pawn -> [] -- Pawn movement must be implemented seperately
  Knight -> [V2 2 1, V2 1 2] >>= mirrorMovesOrth . pure
  Bishop -> bishopMoves
  Rook -> rookMoves
  Queen -> bishopMoves <> rookMoves
  King -> pure <$> (V2 <$> [-1..1] <*> [-1..1])
  where
    bishopMoves = mirrorMovesOrth (join V2 <$> [1..7])
    rookMoves = mirrorMovesDiag (V2 0 <$> [1..7])

mirrorMovesWith :: (V2 a -> V2 a) -> (V2 a -> V2 a) -> [V2 a] -> [[V2 a]]
mirrorMovesWith mirror1 mirror2 moves =
  fmap (<$> moves) $ (.) <$> [id, mirror1] <*> [id, mirror2]

mirrorMovesOrth :: [V2 Int] -> [[V2 Int]]
mirrorMovesOrth =
  mirrorMovesWith (\(V2 x y) -> V2 x (-y)) (\(V2 x y) -> V2 (-x) y)

mirrorMovesDiag :: [V2 Int] -> [[V2 Int]]
mirrorMovesDiag =
  mirrorMovesWith (\(V2 x y) -> V2 y x) (\(V2 x y) -> V2 (-x) (-y))

pawnMovement :: Game -> Square -> [Square]
pawnMovement game@Game{..} from = do
  Piece color Pawn <- maybeToList $ getAtSquare from gameBoard
  asum [ pawnAdvance gameBoard from color
       , pawnCapture gameBoard from color
       , pawnDouble gameBoard from color
       , pawnEnPassant game from
       ]

pawnAdvance :: Board -> Square -> Color -> [Square]
pawnAdvance board from color
  = maybeToList
  . mfilter (isNothing . (`getAtSquare` board))
  . shiftSquare from
  $ pawnAdvanceMove color
  where
    pawnAdvanceMove White = V2 0 (-1)
    pawnAdvanceMove Black = V2 0 1

pawnCapture :: Board -> Square -> Color -> [Square]
pawnCapture board from color
  =   maybeToList
  .   mfilter (capturable . (`getAtSquare` board))
  .   shiftSquare from
  =<< pawnAdvanceMove color
  where
    capturable = (Just (invColor color) ==) . fmap pieceColor
    pawnAdvanceMove White = V2 <$> [-1,1] <*> [-1]
    pawnAdvanceMove Black = V2 <$> [-1,1] <*> [1]

-- TODO: Clean up
pawnDouble :: Board -> Square -> Color -> [Square]
pawnDouble board from color
  = maybeToList
  . mfilter ((valid &&) . isNothing . (`getAtSquare` board))
  . shiftSquare from
  $ pawnDoubleMove color
  where
    valid = unblocked && correctRank
    unblocked
      = isNothing $ (`getAtSquare` board) =<< shiftSquare from (pawnAdvanceMove color)
    pawnAdvanceMove White = V2 0 (-1)
    pawnAdvanceMove Black = V2 0 1
    correctRank = pawnStartRank color == squareRank from
    pawnStartRank White = 6
    pawnStartRank Black = 1
    pawnDoubleMove White = V2 0 (-2)
    pawnDoubleMove Black = V2 0 2

pawnEnPassant :: Game -> Square -> [Square]
pawnEnPassant _board _from = [] -- TODO

castlingMovement :: Game -> Square -> [Square]
castlingMovement _game _from = [] --TODO
