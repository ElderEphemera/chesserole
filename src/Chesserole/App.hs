{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chesserole.App where

import Control.Monad.Reader (MonadIO(..), MonadReader(..), ReaderT(..), asks)
import Data.IORef           (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import GHC.Exts             (coerce)
import GHC.IO.Handle        (Handle)
import System.Process.Typed (Process, getStdin, getStdout)

import SDL

import Chesserole.Chess.Game
import Chesserole.Chess.Moves

--------------------------------------------------------------------------------

data Selection = Selection
  { selSquare :: Square
  , selMoves :: M.Map Square Move
  }

data AppCtx = AppCtx
  { renderer :: Renderer
  , boardTexture :: Texture
  , piecesTexture :: Texture
  , gameRef :: IORef Game
  , selectionRef :: IORef (Maybe Selection)
  , engineProcess :: Process Handle () ()
  }

-- TODO: Capabilities?
newtype App a = App { runApp :: ReaderT AppCtx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppCtx)

askRenderer :: App Renderer
askRenderer = asks renderer

askPiecesTexture :: App Texture
askPiecesTexture = asks piecesTexture

askBoardTexture :: App Texture
askBoardTexture = asks boardTexture

askEngineProcess :: App (Process Handle () ())
askEngineProcess = asks engineProcess

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

getSelection :: App (Maybe Selection)
getSelection = coerce (readIORef . selectionRef)

putSelection :: Maybe Selection -> App ()
putSelection maysel = coerce ((`writeIORef` maysel) . selectionRef)
