// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

namespace Fable.Import.Node.PowerPack

[<AutoOpen>]
module ChildProcess =
    open Fable.Import.Node
    open Fable.Core.JsInterop
    open Fable.Core
    open Fable.PowerPack

    type Stdout = Stdout of string
    type Stderr = Stderr of string

    let private toStr = function
      | U2.Case2(x:Buffer.Buffer) -> x.toString "utf8"
      | U2.Case1(x) -> x

    let exec (cmd:string) (opts:ChildProcess.ExecOptions option) =
      Promise.create(fun res _ ->

        let execOpts =
            match opts with
            | Some(x) -> x
            | None -> createEmpty<ChildProcess.ExecOptions>

        childProcess.exec(cmd, execOpts, (fun e  stdout' stderr' ->
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
