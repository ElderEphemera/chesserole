{-# LANGUAGE RecordWildCards #-}

module Chesserole.Action where

import Control.Monad          (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable          (for_)
import System.IO              (hFlush, hPutStrLn)

import SDL

import Chesserole.App
import Chesserole.Chess.Game
import Chesserole.Chess.Fen
import Chesserole.Render

--------------------------------------------------------------------------------

data Action = Quit | Click Square
  deriving (Eq, Ord, Show)

handleAction :: Action -> App Bool
handleAction Quit = pure False
handleAction (Click sq) = True <$ do
  selSquare <- getSelSquare
  case selSquare of
    Nothing -> do
      Game{..} <- getGame
      for_ (getAtSquare sq gameBoard) $ \_piece -> do
        putSelSquare $ Just sq
        renderBoard
    Just sel -> do
      unless (sq == sel) . modifyGame $ movePiece sel sq
      putSelSquare Nothing
      renderBoard
      getGame >>= liftIO . putStrLn . fen
      getGame >>= engineCommand . ("position fen " <>) . fen
      engineCommand "go"

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
