{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chesserole.Main where

import Control.Concurrent   (threadDelay)
import Control.Monad        (when, unless)
import Control.Monad.Reader (MonadIO(..), MonadReader(..), ReaderT(..))
import Data.Foldable        (for_)
import Data.IORef           (IORef, newIORef, readIORef, writeIORef)
import Data.Traversable     (for)
import Data.Maybe           (mapMaybe)
import Foreign.C.Types      (CInt)
import GHC.Exts             (coerce, fromList)
import Linear               (V4(..))

import SDL
import SDL.Image

import Chesserole.Chess

-- TODO: Organize

--------------------------------------------------------------------------------

main :: IO ()
main = initializeApp >>= runReaderT (runApp app)

initializeApp :: IO AppCtx
initializeApp = do
  initializeAll
  window <- createWindow "Chesserole" $
    defaultWindow { windowInitialSize = V2 800 800 }
  renderer <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  woodgrainTexture <- loadTexture renderer "./assets/woodgrain.png"
  piecesTexture <- loadTexture renderer "./assets/chess.png"
  gameRef <- newIORef initialGame
  selSquareRef <- newIORef Nothing
  return AppCtx{..}

app :: App ()
app = do
  renderBoard
  mainLoop

--------------------------------------------------------------------------------

data AppCtx = AppCtx
  { renderer :: Renderer
  , woodgrainTexture :: Texture
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

askWoodgrainTexture :: App Texture
askWoodgrainTexture = woodgrainTexture <$> ask

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

--------------------------------------------------------------------------------

mainLoop :: App ()
mainLoop = do
  actions <- mapMaybe (eventAction . eventPayload) <$> pollEvents
  when (null actions) . liftIO $ threadDelay 1000
  continue <- and <$> for actions handleAction
  when continue mainLoop

data Action = Quit | Click Square

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
    [ mkSpace x y | y <- [0..7], x <- [0..7], (x + y) `rem` 2 == 0 ]

  selSquare <- getSelSquare
  for_ selSquare $ \sel -> do

    rendererDrawColor renderer $= yellow
    fillRect renderer . Just $ squareSpace sel

    rendererDrawColor renderer $= green
    for_ (validMovesFrom game sel) $ fillRect renderer . Just . squareSpace

  for_ (zip (concat boardPieces) spaces) $ \(maypiece, space) ->
    for_ maypiece $ \piece ->
      copy renderer piecesTexture (Just $ pieceTile piece) (Just space)

  present renderer

--------------------------------------------------------------------------------

pieceTile :: Piece -> Space
pieceTile Piece{..} = Rectangle (P (V2 x y)) (V2 100 100)
  where x = pieceTileCol pieceType
        y = pieceTileRow pieceColor

pieceTileRow :: Color -> CInt
pieceTileRow White = 100
pieceTileRow Black = 0

pieceTileCol :: PieceType -> CInt
pieceTileCol Pawn   = 0
pieceTileCol Knight = 100
pieceTileCol Bishop = 200
pieceTileCol Rook   = 300
pieceTileCol Queen  = 400
pieceTileCol King   = 500

--------------------------------------------------------------------------------

type Space = Rectangle CInt

mkSpace' :: Point V2 CInt -> Space
mkSpace' pt = Rectangle (fmap (100*) pt) (V2 100 100)

mkSpace :: CInt -> CInt -> Space
mkSpace x y = mkSpace' . P $ V2 x y

squareSpace :: Square -> Space
squareSpace (Square' pt) = mkSpace' $ fmap fromIntegral pt

spaces :: [Space]
spaces = [ mkSpace x y | y <- [0..7], x <- [0..7] ]
