// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.Node.PowerPack.NetTest

open Fable.Import.Jest
open Matchers
open Fable.Import.JS
open Fable.Core.JsInterop
open Fable.Import.Node
open Globals

testList "NetHelpers" [
  let withSetup f () =
    let onStringToObj evt (fn:string -> obj) = fn evt
    let mockOnStringToObj = Matcher<string, obj>()
    let mockOnAndOnce = Matcher2<string, string -> obj, obj>(onStringToObj)
    let mockConnect = Matcher<obj, obj>()
    let onObjToObj opts (fn:obj -> obj) = fn opts
    let mockOnObjToObj = Matcher<obj, obj>()
    let mockCreateServer = Matcher2<obj, obj -> obj, obj>(onObjToObj)

    jest.mock("net", fun () -> createObj [
                                "connect" ==> mockConnect.Mock
                                "createServer" ==> mockCreateServer.Mock
    ])

    let nodeHelpers = require.Invoke "../fable/Net.fs"
    f(nodeHelpers, mockConnect, mockCreateServer, mockOnAndOnce, mockOnStringToObj, mockOnObjToObj)

  yield! testFixture withSetup [
    "should expose end with an argument", fun(nodeHelpers, _, _, _, _, _) ->
      let mockSocket = Matcher<string, unit>()
      let c = createObj ["end" ==> mockSocket.Mock]
      nodeHelpers?``end`` (c, "some val") |> ignore
      mockSocket <?> "some val"

    "should expose end without an argument", fun (nodeHelpers, _, _, _, _ , _) ->
      let mockSocket = Matcher<unit, unit>()
      let c = createObj ["end" ==> mockSocket.Mock]
      nodeHelpers?``end`` (c) |> ignore
      mockSocket.LastCalledWith ()

    "should expose onceConnect", fun (nodeHelpers, _, _, mockOnce, mockOnStringToObj, _) ->
      let c = createObj ["once" ==> mockOnce.Mock]
      nodeHelpers?onceConnect (mockOnStringToObj.Mock, c) |> ignore
      mockOnce <??> ("connect", (expect.any Function))
      mockOnStringToObj <?> "connect"

    "should expose onConnect", fun (nodeHelpers, _, _, mockOn, mockOnStringToObj, _) ->
      let c = createObj ["on" ==> mockOn.Mock]
      nodeHelpers?onConnect (mockOnStringToObj.Mock, c) |> ignore
      mockOn <??> ("connect", (expect.any Function))
      mockOnStringToObj <?> "connect"

    "should expose onData", fun (nodeHelpers, _, _, mockOn, mockOnStringToObj, _) ->
      let c = createObj ["on" ==> mockOn.Mock]
      nodeHelpers?onData (mockOnStringToObj.Mock, c) |> ignore
      mockOn <??> ("data", (expect.any Function))
      mockOnStringToObj <?> "data"

    "should expose onError", fun (nodeHelpers, _, _, mockOn, mockOnStringToObj, _) ->
      let c = createObj ["on" ==> mockOn.Mock]
      nodeHelpers?onError (mockOnStringToObj.Mock, c) |> ignore
      mockOn <??> ("error", (expect.any Function))
      mockOnStringToObj <?> "error"

    "should expose connect with NetPath", fun (nodeHelpers, mockConnect, _, _, _, _) ->
      nodeHelpers?connect (createObj ["path" ==> "/var/run/device-scanner.sock" ]) |> ignore
      mockConnect.LastCalledWith (createObj ["path" ==> "/var/run/device-scanner.sock"])

    "should expose createServer", fun (nodeHelpers, _, mockCreateServer, _, _, mockOnObjToObj) ->
      let opts = createObj ["opt1" ==> "val1"]
      nodeHelpers?createServer (opts, mockOnObjToObj.Mock) |> ignore
      mockCreateServer <??> (opts, (expect.any Function))
      mockOnObjToObj <?> opts
  ]
]