{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import EventQueue (EventQueue, readEvent, setSpeed)
import GHC.Generics (Generic)
import GameState (Event (..), GameState, move)
import RenderState (
  BoardInfo,
  RenderMessage,
  RenderState (RenderState),
  render,
  updateMessages,
 )
import RenderState qualified
import UnliftIO (IORef, newIORef, readIORef)

data AppState = AppState {gameState :: GameState, renderState :: RenderState}
  deriving (Generic)

data Env = Env {boardInfo :: BoardInfo, eventQueue :: EventQueue}
  deriving (Generic)

type App = ReaderT (Env, IORef GameState, IORef RenderState) IO

runApp :: Env -> AppState -> App a -> IO a
runApp env initialState app = do
  gameStateRef <- newIORef initialState.gameState
  renderStateRef <- newIORef initialState.renderState
  runReaderT app (env, gameStateRef, renderStateRef)

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
  RenderState{score} <- readIORef renderState
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
  RenderState{gameOver = isGameOver} <- readIORef renderState
  unless isGameOver gameloop

run :: Env -> AppState -> IO ()
run env initialState = runApp env initialState gameloop
