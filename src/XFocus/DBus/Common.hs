{-# LANGUAGE OverloadedStrings #-}
module XFocus.DBus.Common where


import           XFocus.API

import           IPC.DBus


interface :: InterfaceName
interface =
  InterfaceName "me.utf8.xfocus"

task :: ResourceName
task =
  ResourceName "/task"

submit :: Method SubmitRequest SubmitResponse
submit =
  Method {
      methodName = MethodName "Submit"
    }

status :: Method StatusRequest StatusResponse
status =
  Method {
      methodName = MethodName "status"
    }
