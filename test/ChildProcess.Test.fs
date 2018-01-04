// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.Node.PowerPack.ChildProcessTest

open Fable.Import.Jest
open Matchers
open Fable.Import
open Fable.Import.JS
open Fable.Import.Node
open Fable.Core.JsInterop
open Fable.PowerPack
open Fable.Import.Node.PowerPack.ChildProcess
open Fable.Import.Node.Base.NodeJS
open Fable.Import.Node.ChildProcess

testList "ChildProcessHelpers" [
  let withSetup f () =
    let execCallbackHandler _ _ fn = fn (None, Buffer.Buffer.from("buffer1"), "string2")
    let mockChildProcessExec = Matcher3<string, obj, ((Error option * Buffer.Buffer * string) -> unit), unit> (execCallbackHandler)
    let mockChildProcess = createObj ["exec" ==> mockChildProcessExec.Mock]
    jest.mock("child_process", fun () -> mockChildProcess)
    
    let nodeHelpers = Globals.require.Invoke "../fable/ChildProcess.fs"

    f(nodeHelpers, mockChildProcessExec)

  yield! testFixtureAsync withSetup [
    "should call ChildProcess.exec", fun (nodeHelpers, mockExec) ->
      promise {
        let! result = nodeHelpers?exec("command") :?> JS.Promise<Result<(Stdout * Stderr),(ChildProcess.ExecError * Stdout * Stderr)>>
        mockExec <???> ("command", (expect.any Object), (expect.any Function))
        result == Ok (Stdout("buffer1"), Stderr("string2"))
      }
  ]
]

testList "ChildProcessHelpers with error" [
    let withSetup f () =
      let e = Error.Create "An error occurred" :?> ChildProcess.ExecError
      let execCallbackHandler _ _ fn = fn (Some(e), Buffer.Buffer.from("buffer1"), "string2")
      let mockChildProcessExec = Matcher3<string, obj, ((ExecError option * Buffer.Buffer * string) -> unit), unit> (execCallbackHandler)
      let mockChildProcess = createObj ["exec" ==> mockChildProcessExec.Mock]
      jest.mock("child_process", fun () -> mockChildProcess)

      let nodeHelpers = Globals.require.Invoke "../fable/ChildProcess.fs"

      f(nodeHelpers, e)

    yield! testFixtureAsync withSetup [
      "should handle errors", fun (nodeHelpers, e) ->
       
        promise {
          let! result = nodeHelpers?exec("command") :?> JS.Promise<Result<(Stdout * Stderr),(ChildProcess.ExecError * Stdout * Stderr)>>
          result == Result.Error (e, Stdout("buffer1"), Stderr("string2"))
        }
    ]
]