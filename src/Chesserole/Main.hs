{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chesserole.Main where

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
  texture <- loadTexture renderer "./assets/chess.png"
  boardRef <- newIORef initialBoard
  selSquareRef <- newIORef Nothing
  return AppCtx{..}

app :: App ()
app = do
  renderBoard
  mainLoop

--------------------------------------------------------------------------------

data AppCtx = AppCtx
  { renderer :: Renderer
  , texture :: Texture
  , boardRef :: IORef Board -- TODO: Board --> Game
  , selSquareRef :: IORef (Maybe Square)
  }

-- TODO: Capabilities?
newtype App a = App { runApp :: ReaderT AppCtx IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppCtx)

askRenderer :: App Renderer
askRenderer = renderer <$> ask

askTexture :: App Texture
askTexture = texture <$> ask

getBoard :: App Board
getBoard = coerce (readIORef . boardRef)

putBoard :: Board -> App ()
putBoard board = coerce ((`writeIORef` board) . boardRef)

modifyBoard :: (Board -> Board) -> App ()
modifyBoard f = getBoard >>= putBoard . f

getSelSquare :: App (Maybe Square)
getSelSquare = coerce (readIORef . selSquareRef)

putSelSquare :: Maybe Square -> App ()
putSelSquare maysquare = coerce ((`writeIORef` maysquare) . selSquareRef)

--------------------------------------------------------------------------------

-- TODO: Wait between loops
mainLoop :: App ()
mainLoop = do
  events <- pollEvents
--  if null events then pure () else print $ map eventPayload events
  let actions = mapMaybe (eventAction . eventPayload) events
  continue <- and <$> for actions handleAction
  when continue mainLoop

data Action = Quit | Click Square

handleAction :: Action -> App Bool
handleAction Quit = pure False
handleAction (Click sq) = True <$ do
  selSquare <- getSelSquare
  case selSquare of
    Nothing -> do
      board <- getBoard
      for_ (getPiece sq board) $ \_ -> do
        putSelSquare $ Just sq
        renderBoard
    Just sel -> do
      unless (sq == sel) $ modifyBoard (forceMovePiece sel sq)
      putSelSquare Nothing
      renderBoard

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
  renderer <- askRenderer
  let black  = V4 209 139  71 255
      white  = V4 255 206 168 255
      yellow = V4 206 211  46 255

  rendererDrawColor renderer $= black
  clear renderer
  
  rendererDrawColor renderer $= white
  fillRects renderer $ fromList
    [ mkSpace x y | y <- [0..7], x <- [0..7], (x + y) `rem` 2 == 0 ]

  selSquare <- getSelSquare
  for_ selSquare $ \sel -> do
    rendererDrawColor renderer $= yellow
    fillRect renderer . Just $ squareSpace sel
  
  renderPieces

  present renderer

renderPieces :: App ()
renderPieces = do
  AppCtx{..} <- ask
  Board{..} <- getBoard
  for_ (zip (concat boardPieces) spaces) $ \(maypiece, space) ->
    for_ maypiece $ \piece ->
      copy renderer texture (Just $ pieceTile piece) (Just space)

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
