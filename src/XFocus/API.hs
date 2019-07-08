{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XFocus.API (
    API (..)
  , SubmitResponse (..)
  , StatusResponse (..)
  ) where


import           Chronos (Time)

import           GHC.Generics (Generic)

import           XFocus.Task


data API =
  API {
      apiSubmit :: Task -> IO (Maybe SubmitResponse)
    , apiStatus :: IO (Maybe StatusResponse)
    , apiWait :: IO (Maybe TaskResult)
    , apiStop :: StopReason -> IO ()
    }

data SubmitResponse =
  SubmitResponse {
      submitTask :: Task
    , submitTaskStarted :: Time
    } deriving (Eq, Ord, Show, Generic)

data StatusResponse =
  StatusResponse {
      statusTask :: Task
    , statusTaskStarted :: Time
    , statusTaskStatus :: TaskStatus
    } deriving (Eq, Ord, Show, Generic)
