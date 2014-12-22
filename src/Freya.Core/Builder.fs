[<AutoOpen>]
module Freya.Core.Builder

open System

(* Builder *)

type FreyaBuilder () =

    member __.Return (t) : Freya<'T> = 
        fun env -> 
            async.Return (t, env)
    
    member __.ReturnFrom (m: Freya<'T>) = 
        m
    
    member __.Bind (m1: Freya<'T>, m2: 'T -> Freya<'U>) : Freya<'U> = 
        fun s -> 
            async { 
                let! r, s = m1 s
                return! (m2 r) s }
    
    member this.Zero () = 
        this.Return ()
    
    member this.Combine (m1: Freya<unit>, m2: Freya<'T>) : Freya<'T> = 
        this.Bind (m1, fun () -> m2)
    
    member __.TryWith (m: Freya<'T>, handler: exn -> Freya<'T>) : Freya<'T> =
        fun env ->
            try m env
            with e -> (handler e) env
    
    member __.TryFinally (m: Freya<'T>, compensation) : Freya<'T> =
        fun env -> 
            try m env
            finally compensation()
    
    member this.Using (res: #IDisposable, body) =
        this.TryFinally (body res, (fun () -> 
            match res with 
            | null -> () 
            | disp -> disp.Dispose()))
    
    member this.Delay (f) = 
        this.Bind (this.Return (), f)
    
    member this.While (guard, m) =
        match guard () with
        | true -> this.Bind (m, fun () -> this.While (guard, m))
        | _ -> this.Zero ()
        
    member this.For (sequence: seq<_>, body) =
        this.Using (sequence.GetEnumerator (), fun enum -> 
            this.While (enum.MoveNext, this.Delay (fun () -> 
                body enum.Current)))

let freya = new FreyaBuilder ()

(* Helpers *)

let inline internal returnM builder x = 
    (^M: (member Return: 'b -> 'c) (builder, x))

let inline internal bindM builder m f = 
    (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))

let inline internal liftM builder f m =
    let inline ret x = returnM builder (f x)
    bindM builder m ret

let inline internal applyM (builder1: ^M1) (builder2: ^M2) f m =
    bindM builder1 f <| fun f' ->
        bindM builder2 m <| fun m' ->
            returnM builder2 (f' m')