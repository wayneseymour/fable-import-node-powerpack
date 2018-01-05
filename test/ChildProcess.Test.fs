// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.Node.PowerPack.ChildProcessTest

open Fable.Core.JsInterop
open Fable.Import.Jest
open Fable.PowerPack
open Matchers
open Fable.Import.Node

testList "ChildProcessHelpers" [
  let withSetup f () =
    let opts = createEmpty<ChildProcess.ExecOptions>
    opts.encoding <- Some("buffer")
    let p = exec "echo 'Print a message'" (Some(opts))
    f(p)

  yield! testFixtureAsync withSetup [
    "should call ChildProcess.exec", fun (p) ->
      promise {
        let! result = p
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
          let! result = p None
          match result with
            | Error (e:ChildProcess.ExecError, stdout', stderr') -> 
                e.message == "Command failed: echo 'Print a message' 1>&2 && exit 1\nPrint a message\n"
                e.code == 1
                stdout' == Stdout("")
                stderr' == Stderr("Print a message\n")
            | _ -> raise (System.Exception("Command didn't fail as it should have."))
        }
    ]
]