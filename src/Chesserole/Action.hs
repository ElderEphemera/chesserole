{-# LANGUAGE RecordWildCards #-}

module Chesserole.Action where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable          (for_)
import qualified Data.Map.Strict as M
import System.IO              (hFlush, hPutStrLn)

import SDL

import Chesserole.App
import Chesserole.Chess.Game
import Chesserole.Chess.Moves
import Chesserole.Render

--------------------------------------------------------------------------------

data Action = Quit | Click Square
  deriving (Eq, Ord, Show)

handleAction :: Action -> App Bool
handleAction Quit = pure False
handleAction (Click sq) = True <$ do
  maysel <- getSelection
  case maysel of
    Nothing -> do
      game@Game{..} <- getGame
      for_ (getAtSquare sq gameBoard) $ \_piece -> do
        putSelection . Just $ Selection sq (movesFrom game sq)
        renderBoard
    Just (Selection{..}) -> do
      for_ (M.lookup sq selMoves) $ modifyGame . movePiece
      putSelection Nothing
      renderBoard
      --getGame >>= liftIO . putStrLn . fen
      --getGame >>= engineCommand . ("position fen " <>) . fen
      --engineCommand "go"

eventAction :: EventPayload -> Maybe Action
eventAction QuitEvent = Just Quit
eventAction (MouseButtonEvent MouseButtonEventData
             { mouseButtonEventMotion = Released
             , mouseButtonEventButton = ButtonLeft
             , mouseButtonEventPos    = pos 
             }) = Just . Click . pointSquare $ fmap ((`div` 100) . fromEnum) pos
eventAction _ = Nothing

engineCommand :: String -> App ()
engineCommand cmd = do
  h <- askEngineStdin
  liftIO $ hPutStrLn h cmd *> hFlush h
