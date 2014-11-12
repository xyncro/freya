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


(* Traversal *)

let private action a =
    owin {
        do! a.Action
        do! executionI (ExecutionLog.Action {
            Name = a.Id
            Overridden = a.Override.Overridden })

        return a.Next }

let private decision d =
    owin {
        let! result = d.Decision
        let next = (function | true -> d.True | _ -> d.False) result

        do! executionI (ExecutionLog.Decision { 
                    Name = d.Id
                    Overridden = d.Override.Overridden
                    Result = result
                    Next = next })

        return next }

let private handler (h: HandlerNode) =
    owin {
        do! executionI (ExecutionLog.Handler {
            Name = h.Id
            Overridden = h.Override.Overridden })

        return h.Handler }

let private operation o =
    owin {
        do! o.Operation
        do! executionI (ExecutionLog.Operation {
            Name = o.Id })

        return o.Next }

let private traverse (graph: Graph) =
    let rec eval from =
        owin {
            match Map.find from graph with
            | Action a -> return! action a >>= eval
            | Decision d -> return! decision d >>= eval
            | Handler h -> return! handler h
            | Operation o -> return! operation o >>= eval  }

    eval Decisions.ServiceAvailable

(* Invocation *)

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
        do! setPLM (Response.Headers.contentLength) (ContentLength res.Representation.Length)
        do! modLM  (Response.body) (fun b -> b.Write (res.Representation, 0, res.Representation.Length); b) }

let invoke handler =
    owin {    
        let! req = request
        let! res = handler req

        do! write req res }

(* Reification *)

let private nodes =
      actions
    @ decisions
    @ handlers
    @ operations
    
let reifyMachine (machine: MachineMonad) : Pipeline =
    let _, definition = machine Map.empty
    let graph = construct definition nodes

    owin {
        do! initI
        do! setPLM definitionPLens definition
        do! traverse graph >>= invoke

        let! insp = readI

        printfn "insp: %A" insp

        return Halt }