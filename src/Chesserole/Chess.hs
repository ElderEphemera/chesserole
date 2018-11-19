{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- See <https://ghc.haskell.org/trac/ghc/ticket/14630>
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chesserole.Chess where

import Control.Applicative  (ZipList(..))
import Control.Monad        (join, mfilter)
import Data.Char            (toLower)
import Data.Functor.Compose (Compose(..))
import Data.List            (intercalate)
import Data.Maybe           (isNothing, mapMaybe, maybeToList)
import Data.Foldable        (asum)
import Linear               (V2(..))
import Linear.Affine        ((.+^), Point(..))

-- TODO: Organize

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
  deriving (Eq, Show)
  deriving (Functor, Applicative) via (Compose ZipList ZipList)

type Board = Board' (Maybe Piece)

--------------------------------------------------------------------------------

-- TODO: @Point V2 IntMod8@?
newtype Square = Square' { squarePos :: Point V2 Int }
  deriving (Eq, Show)

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

data Move
  = Move Square Square
  | Castle CastleSide

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
