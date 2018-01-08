// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

namespace Fable.Import.Node.PowerPack

[<AutoOpen>]
module Net =
    open Fable.Import.Node
    open Fable.Core

    [<Pojo>]
    type NetPath = {
      path: string
    }

    let onceConnect (fn:unit -> unit) (c:Net.Socket)  = c.once("connect", fn) :?> Net.Socket

    let onConnect (fn:unit -> unit) (c:Net.Socket)  = c.on("connect", fn) :?> Net.Socket

    let connect (x:NetPath) = net.connect x

    let createServer opts serverHandler = net.createServer(opts, serverHandler)
