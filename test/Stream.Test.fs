// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module Fable.Import.Node.PowerPack.StreamTest

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
open Fable.Import.Node.PowerPack

type TestRec = {
  foo:string;
  bar:string;
}

testDone "iterates passthrough string" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.createString None

    p
      |> Stream.iter (toEqual "foo")
      |> Stream.Writable.onFinish (fun () -> d.``done``())
      |> ignore

    p
        |> Writable.``end`` (Some "foo")

testDone "iterates passthrough buffer with opts" <| fun (d) ->
    expect.assertions 1

    let opts = createEmpty<Stream.PassThroughOptions<Buffer.Buffer>>
    let p = PassThrough.create (Some(opts))

    p
      |> Stream.iter (toEqual (buffer.Buffer.from "foo"))
      |> Stream.Writable.onFinish (fun () -> d.``done``())
      |> ignore

    p
        |> Writable.``end`` (Some (buffer.Buffer.from "foo"))

testDone "iterates passthrough buffer without opts" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.create None

    p
      |> Stream.iter (toEqual (buffer.Buffer.from "foo"))
      |> Stream.Writable.onFinish (fun () -> d.``done``())
      |> ignore

    p
        |> Writable.``end`` (Some (buffer.Buffer.from "foo"))

testDone "iterates passthrough obj" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.createObj None

    p
      |> iter (toEqual {foo = "foo"; bar = "bar"})
      |> Writable.onFinish (fun () -> d.``done``())
      |> ignore

    p
        |> Writable.``end`` (Some {foo = "foo"; bar = "bar"})

testDone "error on iterate" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.createObj None
    let err = System.Exception "Big bad error"

    p
        |> iter(fun _ -> raise err)
        |> Readable.onError (fun (e) ->
            e == err
            d.``done``()
        )
        |> ignore
    
    p
        |> Writable.``end`` (Some ("data"))

testDone "transforms passthrough string" <| fun (d) ->
    let p = PassThrough.createString None

    p
      |> Stream.transform None (fun x _ p n ->
        p (Ok (x + "bar"))
        n()
      ) None
      |> Stream.iter (toEqual "foobar")
      |> Stream.Writable.onFinish (fun () -> d.``done``())
      |> ignore

    p
        |> Writable.``end`` (Some "foo")

testDone "map string" <| fun (d) ->
    expect.assertions 1

    let p = Stream.PassThrough.createString None

    p
        |> Writable.write("foo")
        |> Stream.map(fun x -> Ok (x + "bar"))
        |> Stream.iter(fun x ->
            x == "foobar"
            d.``done``()
        )
        |> ignore

testDone "map obj" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.createObj None

    p
        |> Writable.write({foo = "foo"; bar = "bar"})
        |> Stream.map(fun x -> Ok ({x with foo = "baz"}))
        |> Stream.iter(fun x ->
            x == {foo = "baz"; bar = "bar"}
            d.``done``()
        )
        |> ignore

testDone "map string to obj" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.createString None

    p
        |> Writable.write("foo")
        |> Stream.map(fun x -> Ok ({ foo = x; bar = "bar"}))
        |> Stream.iter(fun x ->
            x == {foo = "foo"; bar = "bar"}
            d.``done``()
        )
        |> ignore

testDone "map" <| fun (d) ->
    expect.assertions 1

    let p = stream.PassThrough.Create<Buffer>()

    p.write(buffer.Buffer.from("foo"))
        |> ignore

    p
        |> map(fun x ->
            buffer.Buffer.concat([| x; buffer.Buffer.from "bar" |])
                |> Ok
        )
        |> iter(fun x ->
            x == buffer.Buffer.from "foobar"
            d.``done``()
        )
        |> ignore

testDone "error on map" <| fun (d) ->
    expect.assertions 1

    let p = PassThrough.createString None
    let err = JS.Error.Create "bad mapper"

    p
        |> Writable.write "foo"
        |> map (fun _ -> Error(err))
        |> Readable.onError (fun e ->
            e == err
            d.``done``()
        )
        |> ignore

    Writable.``end`` None p

testDone "reduce" <| fun (d) ->
    expect.assertions 1

    streams {
        yield "foo"
        yield "bar"
    }
        |> Stream.reduce "" (fun acc x ->
            Ok (acc + x)
        )
        |> Stream.iter(fun x ->
            x == "foobar"
            d.``done``()
        )
        |> ignore

testDone "error on reduce" <| fun (d) ->
    expect.assertions 1

    let err = JS.Error.Create "error in reduction"

    streams {
        yield "foo"
        yield "bar"
    }
        |> Stream.reduce "" (fun _ _ ->
            Error(err)
        )
        |> Readable.onError (fun e ->
            e == err
            d.``done``()
        )
        |> ignore

testAsync "should work with streams computation expression" <| fun () ->
    streams {
        yield "foo"

        yield "bar"

        yield "baz"
    }
        |> Stream.reduce "" (fun acc x ->
            Ok (acc + x)
        )
        |> Stream.tap (toEqual "foobarbaz")
        |> streamToPromise

testDone "should handle errors from the stream" <| fun d ->
    expect.assertions 1

    let p = PassThrough.create None

    let jsonStream =
        p
            |> LineDelimitedJson.create()

    let err = JS.Error.Create "Unexpected end of JSON input"

    let fail x =
        d.fail x
            |> ignore

    jsonStream
        |> Readable.onData fail
        |> Readable.onError (toEqual err)
        |> Readable.onEnd (fun () -> d.``done``())
        |> ignore

    p.write(buffer.Buffer.from """{ "food": "bard", """) |> ignore
    p.write(buffer.Buffer.from "\n") |> ignore
    jsonStream.``end``()

testDone "should have a 'data' function" <| fun (d) ->
    expect.assertions 1

    streams {
        yield buffer.Buffer.from """{ "key": "val" }"""
    }
        |> LineDelimitedJson.create()
        |> Readable.onData (toEqual (LineDelimitedJson.Json (createObj [ "key" ==> "val"])))
        |> Readable.onEnd (fun () -> d.``done``())
        |> ignore

testDone "should have an 'error' function" <| fun (d) ->
    streams {
        yield buffer.Buffer.from """{ "food": "bard", """
        yield buffer.Buffer.from "\n"
    }
        |> LineDelimitedJson.create()
        |> Readable.onError (fun e ->
            e == JS.Error.Create "Unexpected end of JSON input"
            d.``done``())
        |> ignore

testDone "should handle multiple errors in a single chunk" <| fun (d) ->
    streams {
        yield (buffer.Buffer.from "{\"Test\":\"bla\"}\n{\"Test\":\n\"bla\"}\n{\"Test2\":\"bla\"}")
    }
        |> LineDelimitedJson.create()
        |> Readable.onError toMatchSnapshot
        |> iter toMatchSnapshot
        |> Writable.onFinish d.``done``
        |> ignore

testList "LineDelimitedJsonStream" [
    let withSetup fn () =
        let p = stream.PassThrough.Create()

        let jsonStream =
            p
                |> LineDelimitedJson.create()

        fn p jsonStream

    yield! testFixtureAsync withSetup [
        "should handle empty JSON obj", fun p jsonStream ->
            p.``end``(buffer.Buffer.from "{}");

            promise {
                let! res = streamToPromise jsonStream

                res == [ LineDelimitedJson.Json createEmpty<obj> ]
            };

        "should handle string", fun p jsonStream ->
            p.``end``(buffer.Buffer.from "\"Info\"");

            promise {
                let! res = streamToPromise jsonStream

                res == [ LineDelimitedJson.Json "Info" ]
            };
        "should handle JSON in a single chunk", fun p jsonStream ->
            p.``end``(buffer.Buffer.from "{ \"foo\": \"bar\", \"bar\": \"baz\" }\n");

            promise {
                let! res = streamToPromise jsonStream

                res == [
                  LineDelimitedJson.Json {
                    foo = "bar";
                    bar = "baz"
                  }
                ]
            };
        "should handle chunks of JSON", fun p jsonStream ->
            p.write(buffer.Buffer.from "{ \"foo\": \"bar\", ") |> ignore
            p.``end``(buffer.Buffer.from "\"bar\": \"baz\" }\n")

            promise {
                let! res = streamToPromise jsonStream

                res == [
                  LineDelimitedJson.Json {
                    foo = "bar";
                    bar = "baz"
                  }
                ]
            };
        "should handle newlines in a string", fun p jsonStream ->
            p.``end``(buffer.Buffer.from (toJson({ foo = "bar\n"; bar = "baz" }) + "\n"))

            promise {
                let! res = streamToPromise jsonStream

                res == [ LineDelimitedJson.Json {
                        foo = "bar\n";
                        bar = "baz"
                    } ]
            };
        "should handle the final json line without a newline", fun p jsonStream ->
            p.``end``(buffer.Buffer.from (toJson({ foo = "bar"; bar = "baz"; })))

            promise {
                let! res = streamToPromise jsonStream

                res == [ LineDelimitedJson.Json {
                    foo = "bar";
                    bar = "baz"
                  } ]
            };
        "should handle multiple records correctly", fun p jsonStream ->
            p.write(buffer.Buffer.from """{"TestRec": { "foo": "bar", """) |> ignore
            p.write(buffer.Buffer.from "\"bar\": \"baz\" }}\n") |> ignore
            p.``end``(buffer.Buffer.from """{"TestRec2": {"baz": "bap"}}""")

            promise {
                let! res = streamToPromise jsonStream

                let exp = [
                    LineDelimitedJson.Json (createObj ["TestRec" ==> createObj [ "foo" ==> "bar"; "bar" ==> "baz" ]]);
                    LineDelimitedJson.Json (createObj ["TestRec2" ==> createObj ["baz" ==> "bap"]]);
                ]

                res == exp
            };
        "should handle two full records in a single chunk", fun p jsonStream ->
            p.``end``(buffer.Buffer.from "{\"TestRec\": { \"foo\": \"bar\", \"bar\": \"baz\" }}\n{\"TestRec2\": {\"baz\": \"bap\"}}\n") |> ignore

            promise {
                let! res = streamToPromise jsonStream

                let exp = [
                    LineDelimitedJson.Json (createObj ["TestRec" ==> createObj [ "foo" ==> "bar"; "bar" ==> "baz" ]]);
                    LineDelimitedJson.Json (createObj ["TestRec2" ==> createObj ["baz" ==> "bap"]]);
                ]

                res == exp
            };
        "should handle errors", fun p jsonStream ->
            p.``end``(buffer.Buffer.from """{"Test:"bla"}""")
            
            promise {
                try
                    let! _ = streamToPromise jsonStream
                    ()
                with
                    | e -> 
                        e == System.Exception "Unexpected token b in JSON at position 8"
            };
    ]
]