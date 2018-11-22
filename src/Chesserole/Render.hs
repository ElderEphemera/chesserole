{-# LANGUAGE RecordWildCards #-}

module Chesserole.Render where

import Control.Monad.Reader (MonadReader(..))
import Data.Foldable        (for_)
import Foreign.C.Types      (CInt)
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
  let yellow = V4 206 211  46 255
      green  = V4  36 186  31 100

  copy renderer boardTexture Nothing Nothing

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
squareToTile (Square file rank) = mkTile (toEnum file) (toEnum rank)

squareTiles :: [Tile]
squareTiles = [ mkTile x y | y <- [0..7], x <- [0..7] ]

pieceTile :: Piece -> Tile
pieceTile Piece{..} = mkTile
  (toEnum . fromEnum $ pieceType)
  (toEnum . fromEnum $ pieceColor)
