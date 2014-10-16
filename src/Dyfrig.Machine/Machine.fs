[<AutoOpen>]
module Dyfrig.Machine.Execution

open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Pipeline


//[<AutoOpen>]
//module Cache =
//    
//    let cache<'T> m =
//        let lens =
//                 dictPLens (string (Guid.NewGuid ()))
//            <?-> boxIso<'T>
//
//        owin {
//            let! value = getPLM lens
//
//            match value with
//            | Some cached ->
//                return cached
//            | _ ->
//                let! created = m
//                do! setPLM lens created
//
//                return created }


[<AutoOpen>]
module internal Invocation =

    let private request =
        owin {
            let! charsets = Charset.negotiated
            let! encodings = Encoding.negotiated
            let! mediaTypes = MediaType.negotiated
            let! languages = Language.negotiated

            return 
                { Charsets = charsets
                  Encodings = encodings
                  MediaTypes = mediaTypes
                  Languages = languages } }

    let private write (req: RepresentationRequest) (res: RepresentationResponse) =
        owin {
            match Option.getOrElseOptionF (fun () -> List.tryFind (fun _ -> true) req.Languages) res.Language with
            | Some _ -> do! setPLM (Response.Headers.contentLanguage) "Language!"
            | _ -> ()

            match Option.getOrElseOptionF (fun () -> List.tryFind (fun _ -> true) req.MediaTypes) res.MediaType with
            | Some _ -> do! setPLM (Response.Headers.contentType) "MediaType!"
            | _ -> ()

            do! setPLM (Response.Headers.contentLength) res.Representation.Length
            do! modLM  (Response.body) (fun b -> b.Write (res.Representation, 0, res.Representation.Length); b) }

    let invoke handler =
        owin {    
            let! req = request
            let! res = handler req

            do! write req res }


[<AutoOpen>]
module Reification =

    let private nodes =
          actions
        @ decisions
        @ handlers
        @ operations
    
    let reifyMachine (machine: MachineMonad) : Pipeline =
        let _, definition = machine Map.empty
        let graph = construct definition nodes

        owin {
            do! setPLM definitionPLens definition
            do! execute graph >>= invoke

            return Halt }
