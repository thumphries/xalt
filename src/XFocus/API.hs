{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XFocus.API (
    API (..)
  , SubmitRequest (..)
  , SubmitResponse (..)
  , StatusRequest (..)
  , StatusResponse (..)
  ) where


import           Chronos (Time)

import           Codec.Serialise (Serialise)

import           GHC.Generics (Generic)

import           XFocus.Task


data API =
  API {
      apiSubmit :: SubmitRequest -> IO (Maybe SubmitResponse)
    , apiStatus :: StatusRequest -> IO (Maybe StatusResponse)
    , apiWait :: IO (Maybe TaskResult)
    , apiStop :: StopReason -> IO ()
    }

data SubmitRequest =
  SubmitRequest {
      submitReqTask :: Task
    } deriving (Eq, Ord, Show, Generic)
instance Serialise SubmitRequest

data SubmitResponse =
  SubmitResponse {
      submitRspTask :: Task
    , submitRspTaskStarted :: Time
    } deriving (Eq, Ord, Show, Generic)
instance Serialise SubmitResponse

data StatusRequest =
  StatusRequest
  deriving (Eq, Ord, Show, Generic)
instance Serialise StatusRequest

data StatusResponse =
  StatusResponse {
      statusRspTask :: Task
    , statusRspTaskStarted :: Time
    , statusRspTaskStatus :: TaskStatus
    } deriving (Eq, Ord, Show, Generic)
instance Serialise StatusResponse
