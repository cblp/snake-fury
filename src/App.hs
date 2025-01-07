{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module App where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Data.Functor.Identity (runIdentity)
import EventQueue (EventQueue, readEvent, setSpeed)
import GHC.Generics (Generic)
import GameState (Event (..), GameState, GameStateF (..), GameStateRef, move)
import RenderState (
  BoardInfo,
  RenderMessage,
  RenderState,
  RenderStateF (RenderState),
  RenderStateRef,
  render,
  updateMessages,
 )
import RenderState qualified
import UnliftIO (newIORef, readIORef)

data AppState = AppState {gameState :: GameState, renderState :: RenderState}
  deriving (Generic)

data Env = Env {boardInfo :: BoardInfo, eventQueue :: EventQueue}
  deriving (Generic)

type App = ReaderT (Env, GameStateRef, RenderStateRef) IO

runApp :: Env -> AppState -> App a -> IO a
runApp env initialState app = do
  gameState <- do
    applePosition <- newIORef $ runIdentity initialState.gameState.applePosition
    movement <- newIORef $ runIdentity initialState.gameState.movement
    snakeSeq <- newIORef $ runIdentity initialState.gameState.snakeSeq
    pure GameState{..}
  renderState <- do
    board <- newIORef $ runIdentity initialState.renderState.board
    gameOver <- newIORef $ runIdentity initialState.renderState.gameOver
    score <- newIORef $ runIdentity initialState.renderState.score
    pure RenderState{..}
  runReaderT app (env, gameState, renderState)

-- | Pull an Event from the queue
pullEvent :: App Event
pullEvent = do
  (Env{eventQueue}, _, _) <- ask
  liftIO $ readEvent eventQueue

updateGameState :: Event -> App [RenderMessage]
updateGameState =
  withReaderT (\(Env{boardInfo}, gameState, _) -> (boardInfo, gameState)) . move

updateRenderState :: [RenderMessage] -> App ()
updateRenderState =
  withReaderT (\(Env{boardInfo}, _, renderState) -> (boardInfo, renderState))
    . updateMessages

render :: App ()
render =
  withReaderT
    (\(Env{boardInfo}, _, renderState) -> (boardInfo, renderState))
    RenderState.render

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: App Int
setSpeedOnScore = do
  (Env{eventQueue}, _, renderState) <- ask
  score <- readIORef renderState.score
  liftIO $ setSpeed score eventQueue

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: App ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >>= pure App.render

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: App ()
gameloop = do
  (_, _, renderState) <- ask
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep
  isGameOver <- readIORef renderState.gameOver
  unless isGameOver gameloop

run :: Env -> AppState -> IO ()
run env initialState = runApp env initialState gameloop
