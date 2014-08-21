namespace Dyfrig.Machine

open System
open System.Globalization
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open FSharpx


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


[<AutoOpen>]
module Monad =

    type Machine = 
        MachineDefinition -> unit * MachineDefinition

    type MachineBuilder () =

        member __.Return _ : Machine =
            fun definition -> (), definition

        member __.ReturnFrom machine : Machine = 
            machine

        member __.Bind (m, k) : Machine = 
            m >> fun (result, definition) -> (k result) definition

        member x.Combine (m1, m2) : Machine = 
            x.Bind (m1, fun () -> m2)

        member internal x.Set (r, lens, value) = 
            x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

    let machine = MachineBuilder ()


[<AutoOpen>]
module Cache =
    
    let cache<'T> m =
        let lens =
            owinEnvPLens<'T> (string (Guid.NewGuid ()))

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
        >?-> isoLens unbox<'T> box
        
    let decisionsPLens k =
        ((fun x -> x.Decisions), (fun d x -> { x with Decisions = d }))
        >-?> mapPLens k

    let handlersPLens k =
        ((fun x -> x.Handlers), (fun h x -> { x with Handlers = h }))
        >-?> mapPLens k

    let definitionLens =
        owinEnvLens<MachineDefinition> "dyfrig.machine.definition"


[<AutoOpen>]
module internal Defaults =

    let defaultAction = 
        owin { 
            return () }

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

    let defaultDecision (p: bool) = 
        owin { 
            return p }

    let defaultHandler code phrase =
        owin {
            do! setPLM Response.StatusCode code
            do! setPLM Response.ReasonPhrase phrase

            return Array.empty<byte> }


[<AutoOpen>]
module internal Logic =

    type Graph =
        Map<string, Node>

    and Node =
        | Action of MachineAction * string       
        | Decision of MachineDecision * (string * string)
        | Handler of MachineHandler


    [<AutoOpen>]
    module Headers =

        let headerEquals h v =
            Option.map ((=) v) >> Option.getOrElse false <!> getPLM (Request.Header h)

        let headerExists h =
            Option.isSome <!> getPLM (Request.Header h)


    [<AutoOpen>]
    module Conditional =

        let private tryParseDate d =
            DateTime.TryParse 
                (d, CultureInfo.InvariantCulture.DateTimeFormat, 
                    DateTimeStyles.AdjustToUniversal)

        let private isValidDate header =
            owin {
                let! header = getPLM (Request.Header header)

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
            headerEquals "If-Match" [ "*" ]

        let ifNoneMatchStar =
            headerEquals "If-None-Match" [ "*" ]

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
            (=) meth <!> getLM Request.Method

        let private getMethods key defaults =
            Option.getOrElse defaults
            <!> getPLM (definitionLens >-?> configPLens<Set<Method>> key)

        let private isValidMethod key defaults =
            owin {
                let! m = getLM Request.Method
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
module internal Construction =

    let private unless l definition otherwise =
        getPL l definition |> Option.getOrElse otherwise

    let private actions definition =
        [ Actions.Delete,                        defaultAction,                Decisions.Deleted
          Actions.Patch,                         defaultAction,                Decisions.RespondWithEntity
          Actions.Post,                          defaultAction,                Decisions.PostRedirect
          Actions.Put,                           defaultAction,                Decisions.Created ]

        |> List.map (fun (name, action, next) ->
            name, Action (unless (actionsPLens name) definition action, next))
    
    let private decisionsInternal =
        [ Decisions.AcceptCharsetExists,         ifAcceptCharsetExists,        (Decisions.CharsetAvailable, Decisions.AcceptEncodingExists)
          Decisions.AcceptEncodingExists,        ifAcceptEncodingExists,       (Decisions.EncodingAvailable, Decisions.Processable)
          Decisions.AcceptExists,                ifAcceptExists,               (Decisions.MediaTypeAvailable, Decisions.AcceptLanguageExists)
          Decisions.AcceptLanguageExists,        ifAcceptLanguageExists,       (Decisions.LanguageAvailable, Decisions.AcceptCharsetExists)
          Decisions.IfMatchExists,               ifMatchExists,                (Decisions.IfMatchStar, Decisions.IfUnmodifiedSinceExists)
          Decisions.IfMatchStar,                 ifMatchStar,                  (Decisions.IfUnmodifiedSinceExists, Decisions.ETagMatchesIf)
          Decisions.IfMatchStarExistsForMissing, ifMatchExists,                (Handlers.PreconditionFailed, Decisions.MethodPut)
          Decisions.IfModifiedSinceExists,       ifModifiedSinceExists,        (Decisions.IfModifiedSinceValidDate, Decisions.MethodDelete)
          Decisions.IfModifiedSinceValidDate,    ifModifiedSinceValidDate,     (Decisions.ModifiedSince, Decisions.MethodDelete)
          Decisions.IfNoneMatch,                 ifNoneMatch,                  (Handlers.NotModified, Handlers.PreconditionFailed)
          Decisions.IfNoneMatchExists,           ifNoneMatchExists,            (Decisions.IfNoneMatchStar, Decisions.IfModifiedSinceExists)
          Decisions.IfNoneMatchStar,             ifNoneMatchStar,              (Decisions.IfNoneMatch, Decisions.ETagMatchesIfNone)
          Decisions.IfUnmodifiedSinceExists,     ifUnmodifiedSinceExists,      (Decisions.IfUnmodifiedSinceValidDate, Decisions.IfNoneMatchExists)
          Decisions.IfUnmodifiedSinceValidDate,  ifUnmodifiedSinceValidDate,   (Decisions.UnmodifiedSince, Decisions.IfNoneMatchExists)
          Decisions.MethodDelete,                ifMethodDelete,               (Actions.Delete, Decisions.MethodPatch)
          Decisions.MethodOptions,               ifMethodOptions,              (Handlers.Options, Decisions.AcceptExists)
          Decisions.MethodPatch,                 ifMethodPatch,                (Actions.Patch, Decisions.PostToExisting)
          Decisions.MethodPut,                   ifMethodPut,                  (Decisions.PutToDifferentUri, Decisions.Existed)
          Decisions.PostToGone,                  ifMethodPost,                 (Decisions.CanPostToGone, Handlers.Gone)
          Decisions.PostToExisting,              ifMethodPost,                 (Actions.Post, Decisions.PutToExisting)
          Decisions.PostToMissing,               ifMethodPost,                 (Decisions.CanPostToMissing, Handlers.NotFound)
          Decisions.PutToExisting,               ifMethodPost,                 (Decisions.Conflict, Decisions.MultipleRepresentations) ]

        |> List.map (fun (name, decision, next) ->
            name, Decision (decision, next))

    let private decisionsPublic definition =
        [ Decisions.Allowed,                     defaultDecision true,         (Decisions.ContentTypeValid, Handlers.Forbidden)
          Decisions.Authorized,                  defaultDecision true,         (Decisions.Allowed, Handlers.Unauthorized)
          Decisions.CanPostToGone,               defaultDecision false,        (Actions.Post, Handlers.Gone)
          Decisions.CanPostToMissing,            defaultDecision true,         (Actions.Post, Handlers.NotFound)
          Decisions.CanPutToMissing,             defaultDecision true,         (Decisions.Conflict, Handlers.NotImplemented)
          Decisions.CharsetAvailable,            ifCharsetAvailable,           (Decisions.AcceptEncodingExists, Handlers.NotAcceptable)
          Decisions.Conflict,                    defaultDecision false,        (Handlers.Conflict, Actions.Put)
          Decisions.ContentTypeKnown,            defaultDecision true,         (Decisions.ValidEntityLength, Handlers.UnsupportedMediaType)
          Decisions.ContentTypeValid,            defaultDecision true,         (Decisions.ContentTypeKnown, Handlers.NotImplemented)
          Decisions.Created,                     defaultDecision true,         (Handlers.Created, Decisions.RespondWithEntity)
          Decisions.Deleted,                     defaultDecision true,         (Decisions.RespondWithEntity, Handlers.Accepted)
          Decisions.EncodingAvailable,           ifEncodingAvailable,          (Decisions.Processable, Handlers.NotAcceptable)
          Decisions.ETagMatchesIf,               ifETagMatchesIf,              (Decisions.IfUnmodifiedSinceExists, Handlers.PreconditionFailed)
          Decisions.ETagMatchesIfNone,           ifETagMatchesIfNone,          (Decisions.IfNoneMatch, Decisions.IfModifiedSinceExists)
          Decisions.Existed,                     defaultDecision false,        (Decisions.MovedPermanently, Decisions.PostToMissing)
          Decisions.Exists,                      defaultDecision true,         (Decisions.IfMatchExists, Decisions.IfMatchStarExistsForMissing)
          Decisions.MethodKnown,                 ifMethodKnown,                (Decisions.UriTooLong, Handlers.UnknownMethod)
          Decisions.LanguageAvailable,           ifLanguageAvailable,          (Decisions.AcceptCharsetExists, Handlers.NotAcceptable)
          Decisions.Malformed,                   defaultDecision false,        (Handlers.Malformed, Decisions.Authorized)
          Decisions.MediaTypeAvailable,          ifMediaTypeAvailable,         (Decisions.AcceptLanguageExists, Handlers.NotAcceptable)
          Decisions.MethodAllowed,               ifMethodAllowed,              (Decisions.Malformed, Handlers.MethodNotAllowed)
          Decisions.ModifiedSince,               ifModifiedSince,              (Decisions.MethodDelete, Handlers.NotModified)
          Decisions.MovedPermanently,            defaultDecision false,        (Handlers.MovedPermanently, Decisions.MovedTemporarily)
          Decisions.MovedTemporarily,            defaultDecision false,        (Handlers.MovedTemporarily, Decisions.PostToGone)
          Decisions.MultipleRepresentations,     defaultDecision false,        (Handlers.MultipleRepresentations, Handlers.OK)
          Decisions.PostRedirect,                defaultDecision false,        (Handlers.SeeOther, Decisions.Created)
          Decisions.Processable,                 defaultDecision true,         (Decisions.Exists, Handlers.UnprocessableEntity)
          Decisions.PutToDifferentUri,           defaultDecision false,        (Handlers.MovedPermanently, Decisions.CanPutToMissing)
          Decisions.RespondWithEntity,           defaultDecision true,         (Decisions.MultipleRepresentations, Handlers.NoContent)
          Decisions.ServiceAvailable,            defaultDecision true,         (Decisions.MethodKnown, Handlers.ServiceUnavailable)
          Decisions.UnmodifiedSince,             ifUnmodifiedSince,            (Handlers.PreconditionFailed, Decisions.IfNoneMatchExists)
          Decisions.UriTooLong,                  defaultDecision false,        (Handlers.UriTooLong, Decisions.MethodAllowed) 
          Decisions.ValidEntityLength,           defaultDecision true,         (Decisions.MethodOptions, Handlers.RequestEntityTooLarge) ]

        |> List.map (fun (name, decision, next) ->
            name, Decision (unless (decisionsPLens name) definition decision, next))

    let private handlers definition =
        [ Handlers.OK,                           defaultHandler 200 "OK"
          Handlers.Created,                      defaultHandler 201 "Created"
          Handlers.Options,                      defaultHandler 201 "Options"
          Handlers.Accepted,                     defaultHandler 202 "Accepted"
          Handlers.NoContent,                    defaultHandler 204 "No Content"
          Handlers.MovedPermanently,             defaultHandler 301 "Moved Permanently"
          Handlers.SeeOther,                     defaultHandler 303 "See Other"
          Handlers.NotModified,                  defaultHandler 304 "Not Modified"
          Handlers.MovedTemporarily,             defaultHandler 307 "Moved Temporarily"
          Handlers.MultipleRepresentations,      defaultHandler 310 "Multiple Representations"
          Handlers.Malformed,                    defaultHandler 400 "Bad Request"
          Handlers.Unauthorized,                 defaultHandler 401 "Unauthorized"
          Handlers.Forbidden,                    defaultHandler 403 "Forbidden"
          Handlers.NotFound,                     defaultHandler 404 "Not Found"
          Handlers.MethodNotAllowed,             defaultHandler 405 "Method Not Allowed"
          Handlers.NotAcceptable,                defaultHandler 406 "Not Acceptable"
          Handlers.Conflict,                     defaultHandler 409 "Conflict"
          Handlers.Gone,                         defaultHandler 410 "Gone"
          Handlers.PreconditionFailed,           defaultHandler 412 "Precondition Failed"
          Handlers.RequestEntityTooLarge,        defaultHandler 413 "Request Entity Too Large"
          Handlers.UriTooLong,                   defaultHandler 414 "URI Too Long"
          Handlers.UnsupportedMediaType,         defaultHandler 415 "Unsupported Media Type"
          Handlers.UnprocessableEntity,          defaultHandler 422 "Unprocessable Entity"
          Handlers.Exception,                    defaultHandler 500 "Internal Server Error"
          Handlers.NotImplemented,               defaultHandler 501 "Not Implemented"
          Handlers.UnknownMethod,                defaultHandler 501 "Unknown Method"
          Handlers.ServiceUnavailable,           defaultHandler 503 "Service Unavailable" ] 
        
        |> List.map (fun (name, handler) ->
            name, Handler (unless (handlersPLens name) definition handler))

    let construct definition =
        [ actions definition
          decisionsInternal
          decisionsPublic definition
          handlers definition ]
        |> List.concat
        |> Map.ofList


[<AutoOpen>]
module internal Execution =

    let execute (graph: Graph) =
        let rec traverse from =
            owin {
                match Map.find from graph with
                | Action (action, next) ->
                    do! action
                    printfn "action: %s" from
                    return! traverse next
                | Decision (decision, choices) ->
                    let! p = decision
                    printfn "decision: %s = %b" from p
                    return! traverse ((p |> function | true -> fst | _ -> snd) choices)
                | Handler handler ->
                    printfn "handler: %s" from
                    return! handler }

        traverse Decisions.ServiceAvailable


[<AutoOpen>]
module Compilation =
    
    let compileMachine (machine: Machine) : OwinMonad<bool> =
        let definition = machine MachineDefinition.empty |> snd
        let graph = construct definition

        owin {
            do! setLM definitionLens definition

            let! body = execute graph

            do! setPLM (Response.Header "Content-Length") [ string body.Length ]
            do! modLM Response.Body (fun x -> x.Write (body, 0, body.Length); x)
        
            return true }
