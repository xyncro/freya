open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Machine

// Request

let meth = cache <| getLM Request.Method
let path = cache <| getLM Request.Path
let body = cache <| getLM Request.Body
let comm = cache <| getPLM (Request.Query "commit")

// Queries

let file = 
    owin {
        let! path = path
        let! comm = comm

        // read

        return Some (Array.empty<byte>) } |> cache

// Actions

let fileCreate = 
    owin {
        let! path = path
        let! body = body

        // create

        return () }

let fileDelete = 
    owin {
        let! path = path
        
        // delete

        return () }
    
// Decisions

let fileExists = Option.isSome <!> file
let fileShow = Option.get <!> file

// Machines

let files =
    machine {
        allowedMethods [ DELETE; GET; POST ]
        exists fileExists
        doPost fileCreate
        doDelete fileDelete
        handleOk fileShow } |> compileMachine

// Pipes

let poweredBy = 
    setPLM (Response.Header "X-PoweredBy") [ "Dyfrig.Machine" ]

let version = 
    setPLM (Response.Header "X-Version") [ "0.1" ]

// Filters

let requireCommit =
    owin {
        let! meth = meth
        let! comm = comm

        match meth, comm with
        | GET, None ->
            let! path = path

            do! setPLM Response.StatusCode 302
            do! setPLM Response.ReasonPhrase "Moved Temporarily"
            do! setPLM (Response.Header "Location") [ sprintf "%s?commit=%s" path "" ]

            return false
        | _ -> 
            return true }


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
