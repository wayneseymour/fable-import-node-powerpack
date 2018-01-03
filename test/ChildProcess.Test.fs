// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.Node.PowerPack.ChildProcessTest

open Fable.Import.Jest
open Matchers
open Fable.Import
open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Node
open Fable.PowerPack
open Fable.Import.Node.PowerPack.ChildProcess
open Globals

testList "ChildProcessHelpers" [
  let withSetup f () =
    let execCallbackHandler _ _ fn = fn (None, "bla", "bla2")
    let mockChildProcessExec = Matcher3<string, obj, ((obj option * string * string) -> unit), unit> (execCallbackHandler)
    let mockChildProcess = createObj ["exec" ==> mockChildProcessExec.Mock]
    jest.mock("child_process", fun () -> mockChildProcess)
    
    let nodeHelpers = require.Invoke "../fable/ChildProcess.fs"
    let p:JS.Promise<Result<(Stdout * Stderr),(ChildProcess.ExecError * Stdout * Stderr)>> = nodeHelpers?exec("command") :?> JS.Promise<Result<(Stdout * Stderr),(ChildProcess.ExecError * Stdout * Stderr)>>

    f(p, mockChildProcessExec)

  yield! testFixtureAsync withSetup [
    "should call ChildProcess.exec", fun (p, mockExec) ->
      promise {
        let! _ = p
        mockExec <???> ("command", (expect.any Object), (expect.any Function))
      }
  ]
]
