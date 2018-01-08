// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.


module Fable.Import.Node.PowerPack.Stream

open Fable.Import.Node
open Fable.Core.JsInterop
open Fable.Core


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

        let push x =
            self.push(Some x)
                |> ignore

        let next x =
            cb x None

        fn chunk enc push next
    )

    match fn2 with
        | Some (f) ->
            transformOpts.flush <- Some(fun cb ->
                let self:Stream.Transform<'a, 'b> = jsThis

                let push x =
                    self.push(Some x)
                        |> ignore

                let fin x =
                    cb x

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
        match fn(chunk) with
            | Ok y ->
                push y
                next None
            | Error y ->
                next (Some y)

    transform None transformer None r

[<PassGenerics>]
let reduce<'read, 'a, 'b when 'read :> Stream.Readable<'a>> (init:'b) (fn:'b -> 'a -> Result<'b, _>) (r:'read) =
    let mutable init':'b = init

    let transformer chunk _ _ next =
        match fn init' chunk with
            | Ok y ->
                init' <- y
                next None
            | Error y ->
                next (Some y)

    let flusher push fin =
        push init'
        fin None

    transform None transformer (Some flusher)  r

[<RequireQualifiedAccess>]
module LineDelimitedJson =
    open Fable.Import.Node.Stream
    open Fable.Import
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

    let private matcher x =
      match Regex.Match(x, "\\n") with
        | m when m.Success -> Some(m.Index)
        | _ -> None

    let private adjustBuff (buff:string) (index:int) =
      let out = buff.Substring(0, index)
      let buff = buff.Substring(index + 1)
      (out, buff)

    let rec private getNextMatch (buff:string) (callback:JS.Error option -> Json option -> unit) (turn:int) =
      let opt = matcher(buff)

      match opt with
        | None ->
          if turn = 0 then
            callback None None
          buff
        | Some(index) ->
          let (out, b) = adjustBuff buff index

          match parser out with
            | Ok(x) -> callback None (Some x)
            | Error(e) -> callback (e |> Some) None

          getNextMatch b callback (turn + 1)

    let create () r =
      let mutable buff = ""

      let optsFn () =
        let opts = createEmpty<TransformOptions<Buffer.Buffer, Json>>
        opts.readableObjectMode <- Some true
        opts

      let dataFn (chunk:Buffer.Buffer) _ push next =
        buff <- getNextMatch
            (buff + chunk.toString("utf-8"))


            (fun err x ->
              if Option.isSome err then
                next(err)
              else
                Option.iter push x
                next None
            )
            0

      let flushFn push fin =
        if buff.Length = 0
          then
            fin None
          else
            match parser buff with
                | Ok(x) ->
                  push x
                  fin None
                | Error(e) ->
                  e |> Some |> fin

      transform (Some optsFn) dataFn (Some flushFn) r
