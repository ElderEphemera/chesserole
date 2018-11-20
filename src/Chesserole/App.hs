{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chesserole.App where

import Control.Monad.Reader (MonadIO(..), MonadReader(..), ReaderT(..))
import Data.IORef           (IORef, readIORef, writeIORef)
import GHC.Exts             (coerce)

import SDL

import Chesserole.Chess.Game

--------------------------------------------------------------------------------

data AppCtx = AppCtx
  { renderer :: Renderer
  , boardTexture :: Texture
  , piecesTexture :: Texture
  , gameRef :: IORef Game
  , selSquareRef :: IORef (Maybe Square)
  }

-- TODO: Capabilities?
newtype App a = App { runApp :: ReaderT AppCtx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppCtx)

askRenderer :: App Renderer
askRenderer = renderer <$> ask

askPiecesTexture :: App Texture
askPiecesTexture = piecesTexture <$> ask

askBoardTexture :: App Texture
askBoardTexture = boardTexture <$> ask

getGame :: App Game
getGame = coerce (readIORef . gameRef)

putGame :: Game -> App ()
putGame game = coerce ((`writeIORef` game) . gameRef)

modifyGame :: (Game -> Game) -> App ()
modifyGame f = getGame >>= putGame . f

getSelSquare :: App (Maybe Square)
getSelSquare = coerce (readIORef . selSquareRef)

putSelSquare :: Maybe Square -> App ()
putSelSquare maysquare = coerce ((`writeIORef` maysquare) . selSquareRef)
