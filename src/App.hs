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

type App = ReaderT (BoardInfo, EventQueue, GameStateRef, RenderStateRef) IO

runApp :: BoardInfo -> EventQueue -> AppState -> App a -> IO a
runApp boardInfo eventQueue initial app = do
  gameState <- do
    applePosition <- newIORef $ runIdentity initial.gameState.applePosition
    movement <- newIORef $ runIdentity initial.gameState.movement
    snakeSeq <- newIORef $ runIdentity initial.gameState.snakeSeq
    pure GameState{..}
  renderState <- do
    board <- newIORef $ runIdentity initial.renderState.board
    gameOver <- newIORef $ runIdentity initial.renderState.gameOver
    score <- newIORef $ runIdentity initial.renderState.score
    pure RenderState{..}
  runReaderT app (boardInfo, eventQueue, gameState, renderState)

-- | Pull an Event from the queue
pullEvent :: App Event
pullEvent = do
  (_, eventQueue, _, _) <- ask
  liftIO $ readEvent eventQueue

updateGameState :: Event -> App [RenderMessage]
updateGameState =
  withReaderT (\(boardInfo, _, gameState, _) -> (boardInfo, gameState)) . move

updateRenderState :: [RenderMessage] -> App ()
updateRenderState =
  withReaderT (\(boardInfo, _, _, renderState) -> (boardInfo, renderState))
    . updateMessages

render :: App ()
render =
  withReaderT
    (\(boardInfo, _, _, renderState) -> (boardInfo, renderState))
    RenderState.render

-- This set the the speed of the game on the score. Notice the constraint give access to all the components.
setSpeedOnScore :: App Int
setSpeedOnScore = do
  (_, eventQueue, _, renderState) <- ask
  score <- readIORef renderState.score
  liftIO $ setSpeed score eventQueue

-- This is one step of the logic: read from the queue and-then update the game state and-then update the render state and-then render
gameStep :: App ()
gameStep = pullEvent >>= updateGameState >>= updateRenderState >>= pure App.render

-- The game loop implementation is provided. To pretty much can read in english.
gameloop :: App ()
gameloop = do
  (_, _, _, renderState) <- ask
  w <- setSpeedOnScore
  liftIO $ threadDelay w
  gameStep
  isGameOver <- readIORef renderState.gameOver
  unless isGameOver gameloop

run :: BoardInfo -> EventQueue -> AppState -> IO ()
run boardInfo eventQueue initialState =
  runApp boardInfo eventQueue initialState gameloop
