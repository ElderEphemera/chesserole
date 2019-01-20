{-# LANGUAGE RecordWildCards #-}

module Chesserole.Render where

import Control.Monad.Reader (MonadReader(..))
import Data.Foldable        (for_)
import qualified Data.Map.Strict as M
import Foreign.C.Types      (CInt)
import Linear               (V4(..))

import SDL

import Chesserole.App
import Chesserole.Chess.Game

--------------------------------------------------------------------------------

renderBoard :: App ()
renderBoard = do
  AppCtx{..} <- ask
  Game{..} <- getGame
  let yellow = V4 206 211  46 255
      green  = V4  36 186  31 100

  copy renderer boardTexture Nothing Nothing

  maysel <- getSelection
  for_ maysel $ \Selection{..} -> do

    rendererDrawColor renderer $= yellow
    fillRect renderer . Just $ squareTile selSquare

    rendererDrawColor renderer $= green
    for_ (M.keys selMoves) $ fillRect renderer . Just . squareTile

  for_ (M.toList gameBoard) $ \(sq, piece) ->
    copy renderer piecesTexture (Just $ pieceTile piece) (Just $ squareTile sq)

  present renderer

--------------------------------------------------------------------------------

type Tile = Rectangle CInt

mkTile' :: Point V2 CInt -> Tile
mkTile' pt = Rectangle (fmap (100*) pt) (V2 100 100)

mkTile :: CInt -> CInt -> Tile
mkTile x y = mkTile' . P $ V2 x y

squareTile :: Square -> Tile
squareTile (Square file rank) = mkTile (toEnum file) (toEnum rank)

pieceTile :: Piece -> Tile
pieceTile Piece{..} = mkTile
  (toEnum . fromEnum $ pieceType)
  (toEnum . fromEnum $ pieceColor)
