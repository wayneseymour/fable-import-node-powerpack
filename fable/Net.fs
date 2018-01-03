// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

namespace Fable.Import.Node.PowerPack

module Net =
    open Fable.Import.Node
    open Fable.Core

    [<Pojo>]
    type NetPath = {
      path: string
    }

    let ``end`` (c:Net.Socket) = function
      | Some(x) -> c.``end``(x)
      | None -> c.``end``()

    let onceConnect (fn:unit -> unit) (c:Net.Socket)  = c.once("connect", fn) :?> Net.Socket

    let onConnect (fn:unit -> unit) (c:Net.Socket)  = c.on("connect", fn) :?> Net.Socket

    let onData (fn:string -> unit) (c:Stream.Stream) = c.on("data", fn) :?> Net.Socket

    let onError (fn:string -> unit) (c:Stream.Stream) = c.on("error", fn) :?> Net.Socket

    let connect (x:NetPath) = Net.connect x

    let createServer opts serverHandler = Net.createServer(opts, serverHandler)
