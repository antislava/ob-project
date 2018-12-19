{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Concurrent
import Network.WebSockets.Snap

import Snap

import qualified Backend.Examples.WebSocketChat.Server as WebSocketChat

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      webSocketChatState <- newMVar WebSocketChat.newServerState
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_WebSocketChat :=> Identity () -> do
          -- runWebSocketsSnap (WebSocketChat.application state)
          runWebSocketsSnap (WebSocketChat.application webSocketChatState)
        BackendRoute_GetData :=> Identity f -> do
          -- -- TODO: Don't use head; and then securely traverse the path.
          -- let fname = contentDir </> T.unpack (head f) <> ".md"
          -- liftIO $ putStrLn fname
          -- liftIO (doesFileExist fname) >>= \case
          --   False ->
          --     putResponse $ setResponseCode 404 emptyResponse
          --   True -> do
          --     content <- liftIO $ T.readFile fname
          --     (_page, html) <- liftIO $ renderStatic $ markdownView content
          --     writeBS html
          -- writeBS "asdf"
          writeText f

  , _backend_routeEncoder = backendRouteEncoder
  }
