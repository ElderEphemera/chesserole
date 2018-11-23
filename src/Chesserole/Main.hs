{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chesserole.Main where

import Control.Concurrent   (threadDelay)
import Control.Monad        (when)
import Control.Monad.Reader (MonadIO(..), ReaderT(..))
import Data.IORef           (newIORef)
import Data.Traversable     (for)
import Data.Maybe           (mapMaybe)
import System.Process.Typed
       (createPipe, setStdin, {-setStdout,-} shell, withProcess)

import SDL
import SDL.Image

import Chesserole.Action
import Chesserole.App
import Chesserole.Chess.Game
import Chesserole.Render

--------------------------------------------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Chesserole" $
    defaultWindow { windowInitialSize = V2 800 800 }
  renderer <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  boardTexture <- loadTexture renderer "./assets/board.png"
  piecesTexture <- loadTexture renderer "./assets/pieces.png"
  gameRef <- newIORef initialGame
  selSquareRef <- newIORef Nothing
  let stockfish = setStdin createPipe {-. setStdout createPipe-} $ shell "stockfish"
  withProcess stockfish $ \engineProcess -> runReaderT (runApp app) AppCtx{..}

app :: App ()
app = do
  engineCommand "uci"
  renderBoard
  mainLoop
  engineCommand "quit"

mainLoop :: App ()
mainLoop = do
  actions <- mapMaybe (eventAction . eventPayload) <$> pollEvents
  when (null actions) . liftIO $ threadDelay 1000
  continue <- and <$> for actions handleAction
  when continue mainLoop
