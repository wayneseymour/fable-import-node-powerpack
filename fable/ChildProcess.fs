// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

namespace Fable.Import.Node.PowerPack

module ChildProcess =
    open Fable.Import.Node
    open Fable.Core.JsInterop
    open Fable.Core
    open Fable.PowerPack
    
    type Stdout = Stdout of string
    type Stderr = Stderr of string

    type ExecOk = Stdout * Stderr
    type ExecErr = ChildProcess.ExecError * Stdout * Stderr

    let private toStr = function
      | U2.Case1(x) -> x
      | U2.Case2(x:Buffer.Buffer) -> x.toString "utf8"

    let exec (cmd:string) =
      Promise.create(fun res _ ->

        let opts = createEmpty<ChildProcess.ExecOptions>

        ChildProcess.exec(cmd, opts, (fun (e, stdout', stderr') ->
          let stdout = stdout' |> toStr |> Stdout
          let stderr = stderr' |> toStr |> Stderr

          match e with
            | Some (e) ->
              (e, stdout, stderr)
                |> Error
                |> res
            | None ->
              (stdout, stderr)
                |> Ok
                |> res
        ))
          |> ignore
      )