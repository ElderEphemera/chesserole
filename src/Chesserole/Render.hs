{-# LANGUAGE RecordWildCards #-}

module Chesserole.Render where

import Control.Monad.Reader (MonadReader(..))
import Data.Foldable        (for_)
import Foreign.C.Types      (CInt)
import GHC.Exts             (fromList)
import Linear               (V4(..))

import SDL

import Chesserole.App
import Chesserole.Chess.Game
import Chesserole.Chess.Moves

--------------------------------------------------------------------------------

renderBoard :: App ()
renderBoard = do
  AppCtx{..} <- ask
  game@Game{gameBoard=Board{..}} <- getGame
  let white  = V4 209 139  71 150
      yellow = V4 206 211  46 255
      green  = V4  36 186  31 100

  copy renderer woodgrainTexture Nothing Nothing

  rendererDrawColor renderer $= white
  fillRects renderer $ fromList
    [ mkTile x y | y <- [0..7], x <- [0..7], (x + y) `rem` 2 == 0 ]

  selSquare <- getSelSquare
  for_ selSquare $ \sel -> do

    rendererDrawColor renderer $= yellow
    fillRect renderer . Just $ squareToTile sel

    rendererDrawColor renderer $= green
    for_ (validMovesFrom game sel) $ fillRect renderer . Just . squareToTile

  for_ (zip (concat boardPieces) squareTiles) $ \(maypiece, space) ->
    for_ maypiece $ \piece ->
      copy renderer piecesTexture (Just $ pieceTile piece) (Just space)

  present renderer

--------------------------------------------------------------------------------

type Tile = Rectangle CInt

mkTile' :: Point V2 CInt -> Tile
mkTile' pt = Rectangle (fmap (100*) pt) (V2 100 100)

mkTile :: CInt -> CInt -> Tile
mkTile x y = mkTile' . P $ V2 x y

squareToTile :: Square -> Tile
squareToTile (Square' pt) = mkTile' $ fmap fromIntegral pt

squareTiles :: [Tile]
squareTiles = [ mkTile x y | y <- [0..7], x <- [0..7] ]

pieceTile :: Piece -> Tile
pieceTile Piece{..} =
  mkTile (pieceTileCol pieceType) (pieceTileRow pieceColor)

pieceTileRow :: Color -> CInt
pieceTileRow White = 1
pieceTileRow Black = 0

pieceTileCol :: PieceType -> CInt
pieceTileCol Pawn   = 0
pieceTileCol Knight = 1
pieceTileCol Bishop = 2
pieceTileCol Rook   = 3
pieceTileCol Queen  = 4
pieceTileCol King   = 5
