// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.PowerPack.Test

open Fable.Import
open Fable.Core
open JsInterop
open Fable.PowerPack
open Fable.Import.Node
open Buffer
open Fable.Import.Jest
open Matchers
open Util
open Fable.Import.Node.PowerPack.Stream
open Fable.Import.Node.PowerPack.LineDelimitedJsonStream


testDone "map" <| fun (d) ->
    expect.assertions 1

    let p = Stream.PassThrough.Create<Buffer>()

    p.write(Buffer.Buffer.from("foo"))
        |> ignore

    p
        |> map(fun x ->
            Buffer.Buffer.concat([| x; Buffer.Buffer.from "bar" |])
                |> Ok
        )
        |> iter(fun x ->
            x == Buffer.Buffer.from "foobar"
            d.``done``()
        )
        |> ignore

type TestRec = {
  foo:string;
  bar:string;
}

testDone "should handle errors from the stream" <| fun d ->
    expect.assertions 1

    let p = Stream.PassThrough.Create()

    let jsonStream = 
        p
            |> getJsonStream()

    let err = JS.Error.Create "Unexpected end of JSON input"

    let fail x =
        d.fail x
            |> ignore



    jsonStream
        .on("data", fail)
        .on("error", (toEqual err))
        .on("end", d.``done``) |> ignore

    p.write(Buffer.Buffer.from """{ "food": "bard", """) |> ignore
    p.write(Buffer.Buffer.from "\n") |> ignore
    jsonStream.``end``()

testList "LineDelimitedJsonStream" [
    let withSetup fn () =
        let p = Stream.PassThrough.Create()

        let jsonStream =
            p
                |> getJsonStream()

        fn p jsonStream

    yield! testFixtureAsync withSetup [
        "should handle empty JSON obj", fun p jsonStream ->
            p.``end``(Buffer.Buffer.from "{}");

            promise {
                let! res = streamToPromise jsonStream

                res == [ Json createEmpty<obj> ]
            };

        "should handle string", fun p jsonStream ->
            p.``end``(Buffer.Buffer.from "\"Info\"");

            promise {
                let! res = streamToPromise jsonStream

                res == [ Json "Info" ]
            };
        "should handle JSON in a single chunk", fun p jsonStream ->
            p.``end``(Buffer.Buffer.from "{ \"foo\": \"bar\", \"bar\": \"baz\" }\n");

            promise {
                let! res = streamToPromise jsonStream

                res == [
                  Json {
                    foo = "bar";
                    bar = "baz"
                  }
                ]
            };
        "should handle chunks of JSON", fun p jsonStream ->
            p.write(Buffer.Buffer.from "{ \"foo\": \"bar\", ") |> ignore
            p.``end``(Buffer.Buffer.from "\"bar\": \"baz\" }\n")

            promise {
                let! res = streamToPromise jsonStream

                res == [
                  Json {
                    foo = "bar";
                    bar = "baz"
                  }
                ]
            };
        "should handle newlines in a string", fun p jsonStream ->
            p.``end``(Buffer.Buffer.from (toJson({ foo = "bar\n"; bar = "baz" }) + "\n"))

            promise {
                let! res = streamToPromise jsonStream

                res == [ Json {
                        foo = "bar\n";
                        bar = "baz"
                    } ]
            };
        "should handle the final json line without a newline", fun p jsonStream ->
            p.``end``(Buffer.Buffer.from (toJson({ foo = "bar"; bar = "baz"; })))

            promise {
                let! res = streamToPromise jsonStream

                res == [ Json {
                    foo = "bar";
                    bar = "baz"
                  } ]
            };

        "should handle multiple records correctly", fun p jsonStream ->
  
            p.write(Buffer.Buffer.from """{"TestRec": { "foo": "bar", """) |> ignore
            p.write(Buffer.Buffer.from "\"bar\": \"baz\" }}\n") |> ignore
            p.``end``(Buffer.Buffer.from """{"TestRec2": {"baz": "bap"}}""")

            promise {
                let! res = streamToPromise jsonStream

                let exp = [
                    Json (createObj ["TestRec" ==> createObj [ "foo" ==> "bar"; "bar" ==> "baz" ]]);
                    Json (createObj ["TestRec2" ==> createObj ["baz" ==> "bap"]]);
                ]

                res == exp
            }
    ]
]
