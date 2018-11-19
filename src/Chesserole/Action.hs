{-# LANGUAGE RecordWildCards #-}

module Chesserole.Action where

import Control.Monad (unless)
import Data.Foldable (for_)

import SDL

import Chesserole.App
import Chesserole.Chess.Game
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
      unless (sq == sel) . modifyGame $ \g@Game{..} ->
        g { gameBoard = forceMovePiece sel sq gameBoard }
      putSelSquare Nothing
      renderBoard
      --getGame >>= liftIO . putStrLn . fen

eventAction :: EventPayload -> Maybe Action
eventAction QuitEvent = Just Quit
eventAction (MouseButtonEvent MouseButtonEventData
             { mouseButtonEventMotion = Released
             , mouseButtonEventButton = ButtonLeft
             , mouseButtonEventPos    = pos 
             }) = Just . Click . Square' $ fmap ((`div` 100) . fromEnum) pos
eventAction _ = Nothing
