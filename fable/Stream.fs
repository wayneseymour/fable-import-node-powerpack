// Copyright (c) 2017 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

namespace Fable.Import.Node.PowerPack

[<AutoOpen>]
module Stream =
    open Fable.Import.Node
    open Fable.Core.JsInterop

    let onError<'a, 'b when 'a :> Stream.Stream> (fn: 'b -> unit) (s:'a) = 
        s.on("error", fn) :?> 'a

    let onData<'a, 'b when 'a :> Stream.Readable<'b>> (fn:'b -> unit) (s:'a) = 
        s.on("data", fn) :?> 'a

    let onEnd<'a, 'b when 'a :> Stream.Writable<'b>> (fn:unit -> unit) (s:'a) =
        s.on("end", fn) :?> 'a

    let ``end``<'a, 'b when 'a :> Stream.Writable<'b>> (x:'b option) (s:'a) =
      match x with
      | Some(x) -> s.``end``(x)
      | None -> s.``end``()

    /// function wrapper for Stream.Transform
    let transform<'read, 'a, 'b when 'read :> Stream.Readable<'a>> optsFn fn fn2 (r:'read) =
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

        let t = Stream.Transform.Create(transformOpts)

        r.pipe(t)

    let map<'read, 'a, 'b when 'read :> Stream.Readable<'a>> (fn:'a -> Result<'b, _>) (r:'read) =
        let transformer chunk _ push next =
            match fn(chunk) with 
                | Ok y -> 
                    push y
                    next None
                | Error y -> 
                    next (Some y)

        transform None transformer None r

    let iter<'a, 'b when 'a :> Stream.Readable<'b> > fn (r:'a) =
        let writableOpts = createEmpty<Stream.WritableOptions<'b>>
        writableOpts.write <- Some(fun chunk _ cb -> 
            
            try 
                fn(chunk)
                cb None
            with err ->
                cb (Some !!err)
        )

        let w = Stream.Writable.Create<'b>(writableOpts)
        
        r.pipe(w)


module LineDelimitedJsonStream =
    open Fable.Import.Node
    open Fable.Import
    open Fable.Core
    open JsInterop
    open System.Text.RegularExpressions
    open Stream

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

    let getJsonStream () r =
      let mutable buff = ""

      let optsFn () = 
        let opts = createEmpty<Stream.TransformOptions<Buffer.Buffer, Json>>
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