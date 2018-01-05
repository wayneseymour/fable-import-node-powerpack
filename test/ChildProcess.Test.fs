// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.Node.PowerPack.ChildProcessTest

open Fable.Import
open Fable.Import.Jest
open Fable.PowerPack
open Matchers
open Fable.Import.Node
open Fable.Import.Node.ChildProcess
open Fable.Import.Node.Base.NodeJS
open Fable.Import
open System

testList "ChildProcessHelpers" [
  let withSetup f () =
    let p = exec "echo 'Print a message'"
    f(p)

  yield! testFixtureAsync withSetup [
    "should call ChildProcess.exec", fun (p) ->
      promise {
        let! result = p :?> JS.Promise<Result<(Stdout * Stderr),(ChildProcess.ExecError * Stdout * Stderr)>>
        result == Ok (Stdout("Print a message\n"), Stderr(""))
      }
  ]
]

testList "ChildProcessHelpers with error" [
    let withSetup f () =
      let p = exec "echo 'Print a message' 1>&2 && exit 1"
      f(p)

    yield! testFixtureAsync withSetup [
      "should handle errors", fun (p) ->
       
        promise {
          let! result = p :?> JS.Promise<Result<(Stdout * Stderr),(ChildProcess.ExecError * Stdout * Stderr)>>
          let err = Fable.Import.JS.Error.Create "Command failed: echo 'Print a message' 1>&2 && exit 1\nPrint a message\n" :?> ChildProcess.ExecError
          err.code <- 1
          match result with
            | Error (e, stdout', stderr') -> 
                e.message == err.message
                e.code == err.code
                stdout' == Stdout("")
                stderr' == Stderr("Print a message\n")
            | _ -> raise (System.Exception("Command didn't fail as it should have."))
        }
    ]
]