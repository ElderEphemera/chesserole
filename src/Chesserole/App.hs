{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chesserole.App where

import Control.Monad.Reader (MonadIO(..), MonadReader(..), ReaderT(..))
import Data.IORef           (IORef, readIORef, writeIORef)
import GHC.Exts             (coerce)
import GHC.IO.Handle        (Handle)
import System.Process.Typed (Process, getStdin, getStdout)

import SDL

import Chesserole.Chess.Game

--------------------------------------------------------------------------------

data AppCtx = AppCtx
  { renderer :: Renderer
  , boardTexture :: Texture
  , piecesTexture :: Texture
  , gameRef :: IORef Game
  , selSquareRef :: IORef (Maybe Square)
  , engineProcess :: Process Handle () ()
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

askEngineProcess :: App (Process Handle () ())
askEngineProcess = engineProcess <$> ask

askEngineStdin :: App Handle
askEngineStdin = getStdin <$> askEngineProcess

askEngineStdOut :: App ()
askEngineStdOut = getStdout <$> askEngineProcess

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
