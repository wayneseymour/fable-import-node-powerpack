// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.


module Fable.Import.Node.PowerPack.Stream

open Fable.Import.Node
open Fable.Core.JsInterop
open Fable.Core
open Fable.Import

let private getReadableState<'a, 'b when 'a :> Stream.Readable<'b>> (r:'a) =
    let readableState:obj = !!r?_readableState
    let objectMode: bool = !!readableState?objectMode
    let encoding: string option = !!readableState?encoding

    (objectMode, encoding)

[<RequireQualifiedAccess>]
module PassThrough =
    let createString (opts:Stream.PassThroughOptions<string> option):Stream.PassThrough<string> =
        let o =
            match opts with
                | Some o -> o
                | None -> createEmpty<Stream.PassThroughOptions<string>>

        o.encoding <- Some Fable.Import.Node.Buffer.BufferEncoding.Utf8

        stream.PassThrough.Create(o)

    let create (opts:Stream.PassThroughOptions<Buffer.Buffer> option):Stream.PassThrough<Buffer.Buffer> =
        match opts with
                | Some o -> stream.PassThrough.Create(o)
                | None -> stream.PassThrough.Create()

    let createObj<'a> (opts:Stream.PassThroughOptions<'a> option):Stream.PassThrough<'a> =
        let o =
            match opts with
                | Some o -> o
                | None -> createEmpty<Stream.PassThroughOptions<'a>>

        o.objectMode <- Some true

        stream.PassThrough.Create(o)

[<RequireQualifiedAccess>]
module Writable =
    let onFinish<'a, 'b when 'a :> Stream.Writable<'b>> (fn:unit -> unit) (s:'a) =
        s.on("finish", fn) :?> 'a

    let writePressure<'a, 'b when 'a :> Stream.Writable<'b>> (chunk:'b) (s:'a) =
        s.write(chunk)

    let write a b =
        writePressure a b
            |> ignore
        b

    let ``end``<'a, 'b when 'a :> Stream.Writable<'b>> (x:'b option) (s:'a) =
      match x with
      | Some(x) -> s.``end``(x)
      | None -> s.``end``()

[<RequireQualifiedAccess>]
module Readable =
    let onData<'a, 'b when 'a :> Stream.Readable<'b>> (fn:'b -> unit) (s:'a) =
        s.on("data", fn) :?> 'a

    let onEnd<'a, 'b when 'a :> Stream.Readable<'b>> (fn:unit -> unit) (s:'a) =
        s.on("end", fn) :?> 'a

    let onError<'a, 'b when 'a :> Stream.Stream> (fn: 'b -> unit) (s:'a) =
        s.on("error", fn) :?> 'a

let iter<'a, 'b when 'a :> Stream.Readable<'b> > fn (r:'a) =
    let writableOpts = createEmpty<Stream.WritableOptions<'b>>
    writableOpts.write <- Some(fun chunk _ cb ->

        try
            fn(chunk)
            cb None
        with err ->
            cb (Some !!err)
    )

    let (objectMode, encoding) = getReadableState r

    if objectMode then
        writableOpts.objectMode <- Some true
    else if Option.isSome encoding then
        writableOpts.decodeStrings <- Some false

    let w = stream.Writable.Create<'b>(writableOpts)

    r.pipe(w)

/// function wrapper for Stream.Transform
[<PassGenerics>]
let transform<'read, 'a, 'b when 'read :> Stream.Readable<'a>> optsFn fn fn2 (r:'read):Stream.Transform<'a, 'b> =
    let transformOpts:Stream.TransformOptions<'a, 'b> =
        match optsFn with
            | Some x -> x()
            | None -> createEmpty<Stream.TransformOptions<_, _>>

    transformOpts.transform <- Some(fun chunk enc cb ->
        let self:Stream.Transform<'a, 'b> = jsThis

        let push = function
            | Ok x -> 
                self.push (Some x)
                    |> ignore
            | Error (x:JS.Error) -> 
                self.emit("error", x)
                    |> ignore

        let next () =
            cb None None

        fn chunk enc push next
    )

    match fn2 with
        | Some (f) ->
            transformOpts.flush <- Some(fun cb ->
                let self:Stream.Transform<'a, 'b> = jsThis

                let push = function
                    | Ok x -> 
                        self.push (Some x)
                            |> ignore
                    | Error (x:JS.Error) -> 
                        self.emit("error", x)
                            |> ignore

                let fin () =
                    cb None

                f push fin
            )
        | None -> ()

    let objectMode, encoding = getReadableState r

    let outType = typeof<'b>

    if objectMode then
        transformOpts.writableObjectMode <- Some true
    else if Option.isSome encoding then
        transformOpts.decodeStrings <- Some false

    match outType.FullName with
        | "string" ->
            transformOpts.encoding <- Some Fable.Import.Node.Buffer.BufferEncoding.Utf8
        | "Fable.Import.Node.Buffer.Buffer" ->
            ()
        | _ ->
            transformOpts.readableObjectMode <- Some true

    let t = stream.Transform.Create(transformOpts)
    r.pipe(t)


[<PassGenerics>]
let map<'read, 'a, 'b when 'read :> Stream.Readable<'a>> (fn:'a -> Result<'b, _>) (r:'read) =
    let transformer chunk _ push next =
        chunk
            |> fn
            |> push

        next()

    transform None transformer None r

[<PassGenerics>]
let tap<'read, 'a, 'b when 'read :> Stream.Readable<'a>> (fn:'a -> 'b) (r:'read) =
    map (fun x -> 
        fn(x)
            |> ignore
        Ok x
    )  r

[<PassGenerics>]
let reduce<'read, 'a, 'b when 'read :> Stream.Readable<'a>> (init:'b) (fn:'b -> 'a -> Result<'b, _>) (r:'read) =
    let mutable init':'b = init

    let transformer chunk _ push next =
        match fn init' chunk with
            | Ok y ->
                init' <- y
            | Error y ->
                push (Error y)

        next()

    let flusher push fin =
        push (Ok init')
        fin()

    transform None transformer (Some flusher)  r

type StreamBuilder() =
    member  __.Yield(v:'a) =
        let s = PassThrough.createObj(None)

        Globals.``process``.nextTick(unbox(fun _ ->
            s.``end``(v)
        ))

        s

    member __.Combine<'a, 'b when 'a :> Stream.PassThrough<'b>>((a:'a), (b:'a)):Stream.PassThrough<'b> =
        let ps = PassThrough.createObj(None)

        let writer x =
            ps.write x
                |> ignore

        Readable.onData writer a
            |> ignore

        Readable.onData writer b
            |> ignore

        Globals.``process``.nextTick(unbox(fun _ ->
            ps.``end``()
        ))

        ps

    member __.Delay(f) =
        f()

/// Computation expression.
/// yielded streams are in object mode
/// so use caution if performance is critical.
let streams = StreamBuilder()

[<RequireQualifiedAccess>]
module LineDelimitedJson =
    open Fable.Import.Node.Stream
    open System.Text.RegularExpressions

    [<Erase>]
    type Json = Json of obj

    let private parser (x:string):Result<Json, JS.Error> =
        try
          x
            |> JS.JSON.parse
            |> Json
            |> Ok
        with
          | ex ->
            Error (!!ex)

    type TransformQueue(parser) =
        let regex = Regex("\\n")
        let mutable incoming = ""
        let mutable queue = []

        member __.Push x =
            incoming <- incoming + x

            while regex.IsMatch(incoming) do
                let m = regex.Match(incoming)

                let record = incoming.Substring(0, m.Index)
                incoming <- incoming.Substring(m.Index + 1)

                queue <- queue @ [parser record]

        member __.ForceFlush () =
            let queue' = queue
            let incoming' = incoming

            queue <- []
            incoming <- ""

            if incoming'.Length > 0 then
                queue' @ [parser incoming']
            else
                []

        member __.Flush () =
            let queue' = queue

            queue <- []

            queue'

    let create () r:Stream.Transform<Buffer.Buffer, Json> =
      let transformQueue = TransformQueue(parser)

      let optsFn () =
        let opts = createEmpty<TransformOptions<Buffer.Buffer, Json>>
        opts.readableObjectMode <- Some true
        opts

      let dataFn (chunk:Buffer.Buffer) _ push next =
        transformQueue.Push(chunk.toString "utf-8")

        transformQueue.Flush()
            |> List.iter push

        next()

      let flushFn push fin =
        transformQueue.ForceFlush()
            |> List.iter push

        fin()

      let s = transform (Some optsFn) dataFn (Some flushFn) r

      Readable.onError(fun _ ->
        r.pipe(s) |> ignore
      ) s
