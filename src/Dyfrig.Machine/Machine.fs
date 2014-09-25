namespace Dyfrig.Machine

open System
open System.Globalization
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Pipeline


[<RequireQualifiedAccess>]
module private Option =

    let getOrElse def x =
        match x with
        | Some x -> x 
        | _ -> def


[<AutoOpen>]
module Definition =

    type MachineDefinition =
        { Actions: Map<string, MachineAction>
          Configuration: Map<string, obj>
          Decisions: Map<string, MachineDecision> 
          Handlers: Map<string, MachineHandler> }

        static member internal empty =
            { Actions = Map.empty
              Configuration = Map.empty
              Decisions = Map.empty
              Handlers = Map.empty }    

    and MachineAction = 
        OwinMonad<unit>

    and MachineDecision = 
        OwinMonad<bool>

    and MachineHandler = 
        OwinMonad<byte []>

    and MachineOperation =
        OwinMonad<unit>


[<AutoOpen>]
module Monad =

    type MachineMonad = 
        MachineDefinition -> unit * MachineDefinition

    type MachineMonadBuilder () =

        member __.Return _ : MachineMonad =
            fun definition -> (), definition

        member __.ReturnFrom machine : MachineMonad = 
            machine

        member __.Bind (m, k) : MachineMonad = 
            m >> fun (result, definition) -> (k result) definition

        member x.Combine (m1, m2) : MachineMonad = 
            x.Bind (m1, fun () -> m2)

        member internal x.Set (r, lens, value) = 
            x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

    let machine = MachineMonadBuilder ()


[<AutoOpen>]
module Cache =
    
    let cache<'T> m =
        let lens =
                 dictPLens (string (Guid.NewGuid ()))
            <?-> boxIso<'T>

        owin {
            let! value = getPLM lens

            match value with
            | Some cached ->
                return cached
            | _ ->
                let! created = m
                do! setPLM lens created

                return created }


[<AutoOpen>]
module internal Lenses =

    let actionsPLens k =
             ((fun x -> x.Actions), (fun a x -> { x with Actions = a }))
        >-?> mapPLens k
    
    let configPLens<'T> k =
             ((fun x -> x.Configuration), (fun c x -> { x with Configuration = c }))
        >-?> mapPLens k
        <?-> boxIso<'T>
        
    let decisionsPLens k =
             ((fun x -> x.Decisions), (fun d x -> { x with Decisions = d }))
        >-?> mapPLens k

    let handlersPLens k =
             ((fun x -> x.Handlers), (fun h x -> { x with Handlers = h }))
        >-?> mapPLens k

    let definitionLens =
             dictLens "dyfrig.machine.definition"
        <--> boxIso<MachineDefinition>


[<AutoOpen>]
module internal Defaults =

    // Actions

    let defaultAction =
        owin { return () }

    // Configuration

    let defaultAllowedMethods =
        Set.ofList
            [ GET
              HEAD ]

    let defaultKnownMethods =
        Set.ofList 
            [ DELETE
              HEAD
              GET
              OPTIONS
              PATCH
              POST
              PUT
              TRACE ]

    // Decisions

    let defaultTrue =
        owin { return true }

    let defaultFalse =
        owin { return false }

    let defaultDecision (decision: bool) = 
        owin { return decision }

    // Handlers

    let defaultHandler code phrase =
        owin {
            do! setPLM Response.statusCode code
            do! setPLM Response.reasonPhrase phrase

            return Array.empty<byte> }

    let defaultOptions =
        owin {
            do! setPLM Response.statusCode 200
            do! setPLM Response.reasonPhrase "Options"
            do! setPLM (Response.header "Access-Control-Allow-Origin") [| "*" |]
            do! setPLM (Response.header "Access-Control-Allow-Headers") [| "Content-Type" |]

            return Array.empty<byte> }


[<AutoOpen>]
module internal Logic =


    [<AutoOpen>]
    module Headers =

        let headerEquals h v =
            Option.map ((=) v) >> Option.getOrElse false <!> getPLM (Request.header h)

        let headerExists h =
            Option.isSome <!> getPLM (Request.header h)


    [<AutoOpen>]
    module Conditional =

        let private tryParseDate d =
            DateTime.TryParse 
                (d, CultureInfo.InvariantCulture.DateTimeFormat, 
                    DateTimeStyles.AdjustToUniversal)

        let private isValidDate header =
            owin {
                let! header = Option.map List.ofArray <!> getPLM (Request.header header)

                match header with
                | Some (h :: _) -> return fst (tryParseDate h)
                | _ -> return false }

        let ifMatchExists =
            headerExists "If-Match"

        let ifNoneMatchExists =
            headerExists "If-None-Match"

        let ifModifiedSinceExists =
            headerExists "If-Modified-Since"

        let ifUnmodifiedSinceExists =
            headerExists "If-Unmodified-Since"

        let ifMatchStar =
            headerEquals "If-Match" [| "*" |]

        let ifNoneMatchStar =
            headerEquals "If-None-Match" [| "*" |]

        let ifModifiedSinceValidDate =
            isValidDate "If-Modified-Since"

        let ifUnmodifiedSinceValidDate =
            isValidDate "If-Unmodified-Since"

        let ifNoneMatch =
            defaultDecision true // IMPLEMENT

        let ifETagMatchesIf =
            defaultDecision true // IMPLEMENT

        let ifETagMatchesIfNone =
            defaultDecision true // IMPLEMENT

        let ifModifiedSince =
            defaultDecision true // IMPLEMENT
            
        let ifUnmodifiedSince =
            defaultDecision true // IMPLEMENT


    [<AutoOpen>]
    module Method =

        let private isMethod meth =
            (=) meth <!> getLM Request.meth

        let private getMethods k d =
            Option.getOrElse d <!> getPLM (definitionLens >-?> configPLens<Set<Method>> k)

        let private isValidMethod key defaults =
            owin {
                let! m = getLM Request.meth
                let! ms = getMethods key defaults

                return Set.contains m ms }

        let ifMethodAllowed =
            isValidMethod Config.AllowedMethods defaultAllowedMethods

        let ifMethodKnown =
            isValidMethod Config.KnownMethods defaultKnownMethods

        let ifMethodDelete =
            isMethod DELETE

        let ifMethodOptions =
            isMethod OPTIONS

        let ifMethodPatch =
            isMethod PATCH

        let ifMethodPost =
            isMethod POST

        let ifMethodPut =
            isMethod PUT


    [<AutoOpen>]
    module Negotiation =

        let ifAcceptExists =
            headerExists "Accept"

        let ifAcceptCharsetExists =
            headerExists "Accept-Charset"

        let ifAcceptEncodingExists =
            headerExists "Accept-Encoding"

        let ifAcceptLanguageExists =
            headerExists "Accept-Language"

        let ifCharsetAvailable =
            defaultDecision true // IMPLEMENT

        let ifEncodingAvailable =
            defaultDecision true // IMPLEMENT

        let ifLanguageAvailable =
            defaultDecision true // IMPLEMENT

        let ifMediaTypeAvailable =
            defaultDecision true // IMPLEMENT


[<AutoOpen>]
module internal Execution =

    type Graph =
        Map<string, Node>

    and Node =
        | Action of ActionNode
        | Decision of DecisionNode
        | Handler of HandlerNode
    
    and ActionNode =
        { Metadata: Metadata
          Action: MachineAction
          OnNext: string }           

    and DecisionNode =
        { Metadata: Metadata
          Decision: MachineDecision
          OnTrue: string
          OnFalse: string }

    and HandlerNode =
        { Metadata: Metadata
          Handler: MachineHandler }

    and Metadata =
        { Name: string
          AllowOverride: bool }

    let executionNodes =
        [ // Actions

          Action { Metadata =
                     { Name = Actions.Delete
                       AllowOverride = true }
                   Action = defaultAction
                   OnNext = Decisions.Deleted }
                   
          Action { Metadata =
                     { Name = Actions.Patch
                       AllowOverride = true }
                   Action = defaultAction
                   OnNext = Decisions.RespondWithEntity }

          Action { Metadata =
                     { Name = Actions.Post
                       AllowOverride = true }
                   Action = defaultAction
                   OnNext = Decisions.PostRedirect }

          Action { Metadata =
                     { Name = Actions.Put
                       AllowOverride = true }
                   Action = defaultAction
                   OnNext = Decisions.Created }

          // Decisions (Allow Override = false)

          Decision { Metadata =
                       { Name = Decisions.AcceptCharsetExists
                         AllowOverride = false }
                     Decision = ifAcceptCharsetExists
                     OnTrue = Decisions.CharsetAvailable
                     OnFalse = Decisions.AcceptEncodingExists }

          Decision { Metadata =
                       { Name = Decisions.AcceptEncodingExists
                         AllowOverride = false }
                     Decision = ifAcceptEncodingExists
                     OnTrue = Decisions.EncodingAvailable
                     OnFalse = Decisions.Processable }

          Decision { Metadata =
                       { Name = Decisions.AcceptExists 
                         AllowOverride = false }
                     Decision = ifAcceptExists
                     OnTrue = Decisions.MediaTypeAvailable
                     OnFalse = Decisions.AcceptLanguageExists }

          Decision { Metadata =
                       { Name = Decisions.AcceptLanguageExists 
                         AllowOverride = false }
                     Decision = ifAcceptLanguageExists
                     OnTrue = Decisions.LanguageAvailable
                     OnFalse = Decisions.AcceptCharsetExists }

          Decision { Metadata =
                       { Name = Decisions.IfMatchExists 
                         AllowOverride = false }
                     Decision = ifMatchExists
                     OnTrue = Decisions.IfMatchStar
                     OnFalse = Decisions.IfUnmodifiedSinceExists }

          Decision { Metadata =
                       { Name = Decisions.IfMatchStar 
                         AllowOverride = false }
                     Decision = ifMatchStar
                     OnTrue = Decisions.IfUnmodifiedSinceExists
                     OnFalse = Decisions.ETagMatchesIf }

          Decision { Metadata =
                       { Name = Decisions.IfMatchStarExistsForMissing 
                         AllowOverride = false }
                     Decision = ifMatchExists
                     OnTrue = Handlers.PreconditionFailed
                     OnFalse = Decisions.MethodPut }

          Decision { Metadata =
                       { Name = Decisions.IfModifiedSinceExists 
                         AllowOverride = false }
                     Decision = ifModifiedSinceExists
                     OnTrue = Decisions.IfModifiedSinceValidDate
                     OnFalse = Decisions.MethodDelete }

          Decision { Metadata =
                       { Name = Decisions.IfModifiedSinceValidDate 
                         AllowOverride = false }
                     Decision = ifModifiedSinceValidDate
                     OnTrue = Decisions.ModifiedSince
                     OnFalse = Decisions.MethodDelete }

          Decision { Metadata =
                       { Name = Decisions.IfNoneMatch 
                         AllowOverride = false }
                     Decision = ifNoneMatch
                     OnTrue = Handlers.NotModified
                     OnFalse = Handlers.PreconditionFailed }

          Decision { Metadata =
                       { Name = Decisions.IfNoneMatchExists 
                         AllowOverride = false }
                     Decision = ifNoneMatchExists
                     OnTrue = Decisions.IfNoneMatchStar
                     OnFalse = Decisions.IfModifiedSinceExists }

          Decision { Metadata =
                       { Name = Decisions.IfNoneMatchStar 
                         AllowOverride = false }
                     Decision = ifNoneMatchStar
                     OnTrue = Decisions.IfNoneMatch
                     OnFalse = Decisions.ETagMatchesIfNone }

          Decision { Metadata =
                       { Name = Decisions.IfUnmodifiedSinceExists 
                         AllowOverride = false }
                     Decision = ifUnmodifiedSinceExists
                     OnTrue = Decisions.IfUnmodifiedSinceValidDate
                     OnFalse = Decisions.IfNoneMatchExists }

          Decision { Metadata =
                       { Name = Decisions.IfUnmodifiedSinceValidDate 
                         AllowOverride = false }
                     Decision = ifUnmodifiedSinceValidDate
                     OnTrue = Decisions.UnmodifiedSince
                     OnFalse = Decisions.IfNoneMatchExists }

          Decision { Metadata =
                       { Name = Decisions.MethodDelete 
                         AllowOverride = false }
                     Decision = ifMethodDelete
                     OnTrue = Actions.Delete
                     OnFalse = Decisions.MethodPatch }

          Decision { Metadata =
                       { Name = Decisions.MethodOptions 
                         AllowOverride = false }
                     Decision = ifMethodOptions
                     OnTrue = Handlers.Options
                     OnFalse = Decisions.AcceptExists }

          Decision { Metadata =
                       { Name = Decisions.MethodPatch 
                         AllowOverride = false }
                     Decision = ifMethodPatch
                     OnTrue = Actions.Patch
                     OnFalse = Decisions.PostToExisting }

          Decision { Metadata =
                       { Name = Decisions.MethodPut 
                         AllowOverride = false }
                     Decision = ifMethodPut
                     OnTrue = Decisions.PutToDifferentUri
                     OnFalse = Decisions.Existed }

          Decision { Metadata =
                       { Name = Decisions.PostToGone 
                         AllowOverride = false }
                     Decision = ifMethodPost
                     OnTrue = Decisions.CanPostToGone
                     OnFalse = Handlers.Gone }

          Decision { Metadata =
                       { Name = Decisions.PostToExisting 
                         AllowOverride = false }
                     Decision = ifMethodPost
                     OnTrue = Actions.Post
                     OnFalse = Decisions.PutToExisting }

          Decision { Metadata =
                       { Name = Decisions.PostToMissing 
                         AllowOverride = false }
                     Decision = ifMethodPost
                     OnTrue = Decisions.CanPostToMissing
                     OnFalse = Handlers.NotFound }

          Decision { Metadata =
                       { Name = Decisions.PutToExisting 
                         AllowOverride = false }
                     Decision = ifMethodPost
                     OnTrue = Decisions.Conflict
                     OnFalse = Decisions.MultipleRepresentations }

          // Decisions (Allow Override = true)

          Decision { Metadata =
                       { Name = Decisions.Allowed
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.ContentTypeValid
                     OnFalse = Handlers.Forbidden }
                      
          Decision { Metadata =
                       { Name = Decisions.Authorized
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.Allowed
                     OnFalse = Handlers.Unauthorized }
                      
          Decision { Metadata =
                       { Name = Decisions.CanPostToGone
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Actions.Post
                     OnFalse = Handlers.Gone }
                      
          Decision { Metadata =
                       { Name = Decisions.CanPostToMissing
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Actions.Post
                     OnFalse = Handlers.NotFound }
                      
          Decision { Metadata =
                       { Name = Decisions.CanPutToMissing
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.Conflict
                     OnFalse = Handlers.NotImplemented }
                      
          Decision { Metadata =
                       { Name = Decisions.CharsetAvailable
                         AllowOverride = true }
                     Decision = ifCharsetAvailable
                     OnTrue = Decisions.AcceptEncodingExists
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.Conflict
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.Conflict
                     OnFalse = Actions.Put }
                      
          Decision { Metadata =
                       { Name = Decisions.ContentTypeKnown
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.ValidEntityLength
                     OnFalse = Handlers.UnsupportedMediaType }
                      
          Decision { Metadata =
                       { Name = Decisions.ContentTypeValid
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.ContentTypeKnown
                     OnFalse = Handlers.NotImplemented }
                      
          Decision { Metadata =
                       { Name = Decisions.Created
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Handlers.Created
                     OnFalse = Decisions.RespondWithEntity }
                      
          Decision { Metadata =
                       { Name = Decisions.Deleted
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.RespondWithEntity
                     OnFalse = Handlers.Accepted }
                      
          Decision { Metadata =
                       { Name = Decisions.EncodingAvailable
                         AllowOverride = true }
                     Decision = ifEncodingAvailable
                     OnTrue = Decisions.Processable
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.ETagMatchesIf
                         AllowOverride = true }
                     Decision = ifETagMatchesIf
                     OnTrue = Decisions.IfUnmodifiedSinceExists
                     OnFalse = Handlers.PreconditionFailed }
                      
          Decision { Metadata =
                       { Name = Decisions.ETagMatchesIfNone
                         AllowOverride = true }
                     Decision = ifETagMatchesIfNone
                     OnTrue = Decisions.IfNoneMatch
                     OnFalse = Decisions.IfModifiedSinceExists }
                      
          Decision { Metadata =
                       { Name = Decisions.Existed
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Decisions.MovedPermanently
                     OnFalse = Decisions.PostToMissing }
                      
          Decision { Metadata =
                       { Name = Decisions.Exists
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.IfMatchExists
                     OnFalse = Decisions.IfMatchStarExistsForMissing }
                      
          Decision { Metadata =
                       { Name = Decisions.MethodKnown
                         AllowOverride = true }
                     Decision = ifMethodKnown
                     OnTrue = Decisions.UriTooLong
                     OnFalse = Handlers.UnknownMethod }
                      
          Decision { Metadata =
                       { Name = Decisions.LanguageAvailable
                         AllowOverride = true }
                     Decision = ifLanguageAvailable
                     OnTrue = Decisions.AcceptCharsetExists
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.Malformed
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.Malformed
                     OnFalse = Decisions.Authorized }
                      
          Decision { Metadata =
                       { Name = Decisions.MediaTypeAvailable
                         AllowOverride = true }
                     Decision = ifMediaTypeAvailable
                     OnTrue = Decisions.AcceptLanguageExists
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.MethodAllowed
                         AllowOverride = true }
                     Decision = ifMethodAllowed
                     OnTrue = Decisions.Malformed
                     OnFalse = Handlers.MethodNotAllowed }
                      
          Decision { Metadata =
                       { Name = Decisions.ModifiedSince
                         AllowOverride = true }
                     Decision = ifModifiedSince
                     OnTrue = Decisions.MethodDelete
                     OnFalse = Handlers.NotModified }
                      
          Decision { Metadata =
                       { Name = Decisions.MovedPermanently
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.MovedPermanently
                     OnFalse = Decisions.MovedTemporarily }
                      
          Decision { Metadata =
                       { Name = Decisions.MovedTemporarily
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.MovedTemporarily
                     OnFalse = Decisions.PostToGone }
                      
          Decision { Metadata =
                       { Name = Decisions.MultipleRepresentations
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.MultipleRepresentations
                     OnFalse = Handlers.OK }
                      
          Decision { Metadata =
                       { Name = Decisions.PostRedirect
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.SeeOther
                     OnFalse = Decisions.Created }
                      
          Decision { Metadata =
                       { Name = Decisions.Processable
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.Exists
                     OnFalse = Handlers.UnprocessableEntity }
                      
          Decision { Metadata =
                       { Name = Decisions.PutToDifferentUri
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.MovedPermanently
                     OnFalse = Decisions.CanPutToMissing }
                      
          Decision { Metadata =
                       { Name = Decisions.RespondWithEntity
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.MultipleRepresentations
                     OnFalse = Handlers.NoContent }
                      
          Decision { Metadata =
                       { Name = Decisions.ServiceAvailable
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.MethodKnown
                     OnFalse = Handlers.ServiceUnavailable }
                      
          Decision { Metadata =
                       { Name = Decisions.UnmodifiedSince
                         AllowOverride = true }
                     Decision = ifUnmodifiedSince
                     OnTrue = Handlers.PreconditionFailed
                     OnFalse = Decisions.IfNoneMatchExists }
                      
          Decision { Metadata =
                       { Name = Decisions.UriTooLong
                         AllowOverride = true }
                     Decision = defaultFalse
                     OnTrue = Handlers.UriTooLong
                     OnFalse = Decisions.MethodAllowed }

          Decision { Metadata =
                       { Name = Decisions.ValidEntityLength
                         AllowOverride = true }
                     Decision = defaultTrue
                     OnTrue = Decisions.MethodOptions
                     OnFalse = Handlers.RequestEntityTooLarge }

          // Handlers
                     
          Handler { Metadata =
                      { Name = Handlers.OK
                        AllowOverride = true }
                    Handler = defaultHandler 200 "OK" }
                    
          Handler { Metadata =
                      { Name = Handlers.Options
                        AllowOverride = true }
                    Handler = defaultOptions }

          Handler { Metadata =
                      { Name = Handlers.Created
                        AllowOverride = true }
                    Handler = defaultHandler 201 "Created" }

          Handler { Metadata =
                      { Name = Handlers.Accepted
                        AllowOverride = true }
                    Handler = defaultHandler 202 "Accepted" }

          Handler { Metadata =
                      { Name = Handlers.NoContent
                        AllowOverride = true }
                    Handler = defaultHandler 204 "No Content" }

          Handler { Metadata =
                      { Name = Handlers.MovedPermanently
                        AllowOverride = true }
                    Handler = defaultHandler 301 "Moved Permanently" }

          Handler { Metadata =
                      { Name = Handlers.SeeOther
                        AllowOverride = true }
                    Handler = defaultHandler 303 "See Other" }

          Handler { Metadata =
                      { Name = Handlers.NotModified
                        AllowOverride = true }
                    Handler = defaultHandler 304 "Not Modified" }

          Handler { Metadata =
                      { Name = Handlers.MovedTemporarily
                        AllowOverride = true }
                    Handler = defaultHandler 307 "Moved Temporarily" }

          Handler { Metadata =
                      { Name = Handlers.MultipleRepresentations
                        AllowOverride = true }
                    Handler = defaultHandler 310 "Multiple Representations" }

          Handler { Metadata =
                      { Name = Handlers.Malformed
                        AllowOverride = true }
                    Handler = defaultHandler 400 "Bad Request" }

          Handler { Metadata =
                      { Name = Handlers.Unauthorized
                        AllowOverride = true }
                    Handler = defaultHandler 401 "Unauthorized" }

          Handler { Metadata =
                      { Name = Handlers.Forbidden
                        AllowOverride = true }
                    Handler = defaultHandler 403 "Forbidden" }

          Handler { Metadata =
                      { Name = Handlers.NotFound
                        AllowOverride = true }
                    Handler = defaultHandler 404 "Not Found" }

          Handler { Metadata =
                      { Name = Handlers.MethodNotAllowed
                        AllowOverride = true }
                    Handler = defaultHandler 405 "Method Not Allowed" }

          Handler { Metadata =
                      { Name = Handlers.NotAcceptable
                        AllowOverride = true }
                    Handler = defaultHandler 406 "Not Acceptable" }

          Handler { Metadata =
                      { Name = Handlers.Conflict
                        AllowOverride = true }
                    Handler = defaultHandler 409 "Conflict" }

          Handler { Metadata =
                      { Name = Handlers.Gone
                        AllowOverride = true }
                    Handler = defaultHandler 410 "Gone" }

          Handler { Metadata =
                      { Name = Handlers.PreconditionFailed
                        AllowOverride = true }
                    Handler = defaultHandler 412 "Precondition Failed" }

          Handler { Metadata =
                      { Name = Handlers.RequestEntityTooLarge
                        AllowOverride = true }
                    Handler = defaultHandler 413 "Request Entity Too Large" }

          Handler { Metadata =
                      { Name = Handlers.UriTooLong
                        AllowOverride = true }
                    Handler = defaultHandler 414 "URI Too Long" }

          Handler { Metadata =
                      { Name = Handlers.UnsupportedMediaType
                        AllowOverride = true }
                    Handler = defaultHandler 415 "Unsupported Media Type" }

          Handler { Metadata =
                      { Name = Handlers.UnprocessableEntity
                        AllowOverride = true }
                    Handler = defaultHandler 422 "Unprocessable Entity" }

          Handler { Metadata =
                      { Name = Handlers.Exception
                        AllowOverride = true }
                    Handler = defaultHandler 500 "Internal Server Error" }

          Handler { Metadata =
                      { Name = Handlers.NotImplemented
                        AllowOverride = true }
                    Handler = defaultHandler 501 "Not Implemented" }

          Handler { Metadata =
                      { Name = Handlers.UnknownMethod
                        AllowOverride = true }
                    Handler = defaultHandler 501 "Unknown Method" }

          Handler { Metadata =
                      { Name = Handlers.ServiceUnavailable
                        AllowOverride = true }
                    Handler = defaultHandler 503 "Service Unavailable" } ]

    let construct () =
        executionNodes
        |> List.map (fun node ->
            match node with
            | Action action ->
                action.Metadata.Name, Action action
            | Decision decision ->
                decision.Metadata.Name, Decision decision
            | Handler handler ->
                handler.Metadata.Name, Handler handler)
        |> Map.ofList

    let execute (graph: Graph) =
        let rec traverse from =
            owin {
                match Map.find from graph with
                | Action action ->
                    do! action.Action
                    printfn "action: %s" from
                    return! traverse action.OnNext
                | Decision decision ->
                    let! p = decision.Decision
                    let next = p |> function | true -> decision.OnTrue | _ -> decision.OnFalse
                    printfn "decision: %s = %b" from p
                    return! traverse next
                | Handler handler ->
                    printfn "handler: %s" from
                    return! handler.Handler }

        traverse Decisions.ServiceAvailable


[<AutoOpen>]
module Reification =
    
    let reifyMachine (machine: MachineMonad) : Pipeline =
        let definition = machine MachineDefinition.empty |> snd
        let graph = construct () // definition

        owin {
            do! setLM definitionLens definition

            let! body = execute graph

            do! setPLM (Response.header "Content-Length") [| string body.Length |]
            do! modLM Response.body (fun x -> x.Write (body, 0, body.Length); x)
        
            return Halt }
