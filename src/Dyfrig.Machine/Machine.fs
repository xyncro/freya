namespace Dyfrig.Machine

open System
open System.Globalization
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Pipeline


module A = Actions
module C = Configuration
module D = Decisions
module H = Handlers
module O = Operations


[<RequireQualifiedAccess>]
module private Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def

    let getOrElseOptionF f =
        function | Some x -> Some x
                 | _ -> f ()


[<AutoOpen>]
module Data =

    type MachineRepresentationRequest =
        { Charsets: SpecifiedCharset list
          Encodings: SpecifiedEncoding list
          MediaTypes: SpecifiedMediaRange list
          Languages: CultureInfo list }

        static member Create charsets encodings mediaTypes languages =
            { Charsets = charsets
              Encodings = encodings
              MediaTypes = mediaTypes
              Languages = languages }

    type MachineRepresentationResponse =
        { Charset: SpecifiedCharset option
          Encoding: SpecifiedEncoding option
          MediaType: SpecifiedMediaRange option
          Language: CultureInfo option
          Representation: byte [] }

        static member Default representation =
            { Charset = None
              Encoding = None
              MediaType = None
              Language = None
              Representation = representation }


[<AutoOpen>]
module Definition =

    (* Signatures
        
       Common monadic signatures for the building blocks of Machine
       Definitions. Represent functions that the user of Machine should implement
       when overriding the defaults. *)

    type MachineAction = 
        OwinMonad<unit>

    type MachineDecision = 
        OwinMonad<bool>

    type MachineHandler = 
        MachineRepresentationRequest -> OwinMonad<MachineRepresentationResponse>

    type MachineOperation =
        OwinMonad<unit>

    (* Definition
        
       A Definition of a Machine, encoded as the defaults to override
       and the functions (given the previously defined Signatures) provided
       to override them. *)

    type MachineDefinition =
        Map<string, MachineOverride>

    and MachineOverride =
        | Action of MachineAction
        | Configuration of obj
        | Decision of MachineDecision
        | Handler of MachineHandler


[<AutoOpen>]
module Monad =

    (* Monad
        
       The monad to give Machine the declarative computation
       expression syntax for specifying Machine Definitions. Specific strongly 
       typed custom operations are defined in Machine.Syntax.fs. *)

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
module internal Patterns =

    (* Patterns
        
       Active patterns for discriminating between varying kinds of 
       Override within a Machine Definition. *)

    let (|Action|) =
        function | Action x -> Some x
                 | _ -> None

    let (|Configuration|) =
        function | Configuration x -> Some x 
                 | _ -> None
        
    let (|Decision|) =
        function | Decision x -> Some x
                 | _ -> None

    let (|Handler|) =
        function | Handler x -> Some x
                 | _ -> None


[<AutoOpen>]
module internal Lenses =

    (* Lenses
        
       Partial lenses (Aether form - see https://github.com/xyncro/aether) 
       to the Machine Definition within an OWIN monad (see Dyfrig.Core),
       and to aspects of the machine definition. *)

    let definitionPLens =
             dictPLens "dyfrig.machine.definition"
        <?-> boxIso<MachineDefinition>

    let actionPLens k =
             mapPLens k
        <??> ((|Action|), Action)
    
    let configurationPLens<'T> k =
             mapPLens k
        <??> ((|Configuration|), Configuration)
        <?-> boxIso<'T>
        
    let decisionPLens k =
             mapPLens k
        <??> ((|Decision|), Decision)

    let handlerPLens k =
             mapPLens k
        <??> ((|Handler|), Handler)  


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
module internal Defaults =

    (* Actions

       The overridable default Action (called on active methods such as
       DELETE, POST, etc. By default the action is no-op, as a safe default. *)

    let action =
        returnM ()

    (* Configuration

       Overridable configuration properties. These should be overridden
       according to the resource design. Some of these properties such
       as mediaTypesAvailable should almost always be overridden. *)

    let defaultCharsets =
        [ SpecifiedCharset.Named "iso-8859-1" ]

    let defaultEncodings =
        [ SpecifiedEncoding.Identity ]

    let defaultLanguages =
        List.empty<CultureInfo>

    let defaultMediaTypes =
        List.empty<SpecifiedMediaRange>
        
    let defaultMethods =
        Set.ofList
            [ GET
              HEAD ]

    let defaultMethodsKnown =
        Set.ofList 
            [ DELETE
              HEAD
              GET
              OPTIONS
              PATCH
              POST
              PUT
              TRACE ]

    (* Decisions

       A configurable default decision handler to be overridden by
       resource specific logic. *)

    let decision (decision: bool) =
        returnM decision

    (* Handlers

       A default handler which returns only an empty response, assuming no modifications
       to the negotiated charset, encoding, etc. and which should
       be overridden according to the resource design and the methods, etc.
       handled. *)

    let handler _ =
        returnM { Charset = None
                  Encoding = None
                  MediaType = None
                  Language = None
                  Representation = Array.empty }

    (* Operations

       Default operations to ensure a basic correct status code and response, 
       plus specialized handlers for specific operations within the resource execution. *)

    let operation statusCode reasonPhrase =
           setPLM Response.statusCode statusCode
        *> setPLM Response.reasonPhrase reasonPhrase

    // TODO: Make this inspect available methods, origins, etc. properly.
    // TODO: Break allowed origin in to an earlier operation

    let options =
           operation 200 "Options"
        *> setPLM (Response.headersKey "Access-Control-Allow-Origin") [| "*" |]
        *> setPLM (Response.headersKey "Access-Control-Allow-Headers") [| "Content-Type" |]


[<AutoOpen>]
module internal LogicOld =

    [<AutoOpen>]
    module Conditional =                        

        let ifETagMatchesIf =
            decision true // IMPLEMENT

        let ifETagMatchesIfNone =
            decision true // IMPLEMENT


[<AutoOpen>]
module internal Logic =

    let inline private (!?) x =
        Option.isSome <!> x

    let inline private (!.) x =
        List.isEmpty >> not <!> x

    let inline private (=?) x y =
        (=) y <!> x

    let inline private (?>) (x, y) f =
        (fun x y -> f (x, y)) <!> x <*> y


    [<AutoOpen>]
    module ContentNegotiation =

        [<RequireQualifiedAccess>]
        module Charset =

            let private request =
                getPLM Request.Headers.acceptCharset

            let private supported =
                getPLM (definitionPLens >??> configurationPLens C.CharsetsSupported)

            let private negotiation =
                function | Some requested, Some available -> negotiateAcceptCharset available requested
                         | Some requested, _ -> negotiateAcceptCharset defaultCharsets requested
                         | _, Some available -> available
                         | _, _ -> defaultCharsets

            let requested =
                !? request

            let negotiated =
                (request, supported) ?> negotiation

            let negotiable =
                !. negotiated


        [<RequireQualifiedAccess>]
        module Encoding =

            let private request =
                getPLM Request.Headers.acceptEncoding

            let private supported =
                getPLM (definitionPLens >??> configurationPLens C.EncodingsSupported)

            let private negotiation =
                function | Some requested, Some available -> negotiateAcceptEncoding available requested
                         | Some requested, _ -> negotiateAcceptEncoding defaultEncodings requested
                         | _, Some available -> available
                         | _, _ -> defaultEncodings

            let requested =
                !? request

            let negotiated =
                (request, supported) ?> negotiation

            let negotiable =
                !. negotiated 


        [<RequireQualifiedAccess>]
        module Language =
        
            let private request =
                getPLM Request.Headers.acceptLanguage

            let private supported =
                getPLM (definitionPLens >??> configurationPLens C.LanguagesSupported)

            let private negotiation =
                function | Some requested, Some available -> negotiateAcceptLanguage available requested
                         | Some requested, _ -> negotiateAcceptLanguage defaultLanguages requested
                         | _, Some available -> available
                         | _, _ -> defaultLanguages

            let requested =
                !? request

            let negotiated =
                (request, supported) ?> negotiation

            let negotiable =
                !. negotiated


        [<RequireQualifiedAccess>]
        module MediaType =

            let private request =
                getPLM Request.Headers.accept

            let private supported =
                getPLM (definitionPLens >??> configurationPLens C.MediaTypesSupported)

            let private negotiation =
                function | Some requested, Some available -> negotiateAccept available requested
                         | Some requested, _ -> negotiateAccept defaultMediaTypes requested
                         | _, Some available -> available
                         | _, _ -> defaultMediaTypes

            let requested =
                !? request

            let negotiated =
                (request, supported) ?> negotiation

            let negotiable =
                !. negotiated


    [<AutoOpen>]
    module Control =

        [<RequireQualifiedAccess>]
        module IfMatch =

            let private ifMatch =
                getPLM Request.Headers.ifMatch

            let requested =
                !? ifMatch

            let any =
                ifMatch =? Some IfMatch.Any


        [<RequireQualifiedAccess>]
        module IfModifiedSince =

            let private ifModifiedSince =
                getPLM Request.Headers.ifModifiedSince

            let private lastModified =
                getPLM (definitionPLens >??> configurationPLens C.LastModified)

            let requested =
                !? ifModifiedSince

            let valid =
                    Option.map ((>) DateTime.UtcNow) >> Option.getOrElse false
                <!> ifModifiedSince

            let modified =
                owin {
                    let! lm = lastModified
                    let! ms = ifModifiedSince

                    match lm, ms with
                    | Some lm, Some ms -> return! ((<) ms) <!> lm
                    | _ -> return false }


        [<RequireQualifiedAccess>]
        module IfNoneMatch =

            let private ifNoneMatch =
                getPLM Request.Headers.ifNoneMatch

            let requested =
                !? ifNoneMatch

            let any =
                ifNoneMatch =? Some IfNoneMatch.Any


        [<RequireQualifiedAccess>]
        module IfUnmodifiedSince =

            let private ifUnmodifiedSince =
                getPLM Request.Headers.ifUnmodifiedSince

            let private lastModified =
                getPLM (definitionPLens >??> configurationPLens C.LastModified)

            let requested =
                !? ifUnmodifiedSince

            let valid =
                    Option.map ((>) DateTime.UtcNow) >> Option.getOrElse false
                <!> ifUnmodifiedSince

            let modified =
                owin {
                    let! lm = lastModified
                    let! us = ifUnmodifiedSince

                    match lm, us with
                    | Some lm, Some us -> return! ((>) us) <!> lm
                    | _ -> return true }


    [<AutoOpen>]
    module Request =

        [<RequireQualifiedAccess>]
        module Method =

            let private meth =
                getLM Request.meth

            let private methodsKnown =
                getPLM (definitionPLens >??> configurationPLens C.MethodsKnown)

            let private negotiateKnown =
                function | x, Some y -> Set.contains x y
                         | x, _ -> Set.contains x defaultMethodsKnown

            let private methodsSupported =
                getPLM (definitionPLens >??> configurationPLens C.MethodsSupported)

            let private negotiateSupported =
                function | x, Some y -> Set.contains x y
                         | x, _ -> Set.contains x defaultMethods

            let known =
                (meth, methodsKnown) ?> negotiateKnown

            let supported =
                (meth, methodsSupported) ?> negotiateSupported

            let delete =
                meth =? DELETE 

            let getOrHead =
                    (fun x -> x = GET || x = HEAD) 
                <!> meth

            let options =
                meth =? OPTIONS

            let patch =
                meth =? PATCH

            let post =
                meth =? POST

            let put =
                meth =? PUT


[<AutoOpen>]
module internal Execution =

    (* Graph
        
       Execution runs as a graph of nodes of specific meaning,
       Each node may (depending on type) run some kind of action and
       then provide a way of indicating which node in the graph should
       be invoked next (forming the essential characteristic of processing
       requests as a statemachine).  *)

    type Graph =
        Map<string, Node>

    and Node =
        | Action of ActionNode
        | Decision of DecisionNode
        | Handler of HandlerNode
        | Operation of OperationNode
    
    and ActionNode =
        { Id: string
          Override: Override
          Action: MachineAction
          Next: string }

    and DecisionNode =
        { Id: string
          Override: Override
          Decision: MachineDecision
          True: string
          False: string }

    and HandlerNode =
        { Id: string
          Override: Override
          Handler: MachineHandler }

    and OperationNode =
        { Id: string
          Operation: MachineOperation
          Next: string }

    (* Override
       
       Override data is used to be able to provide sensible runtime
       introspection and debugging capabilities,such as integration with future 
       Dyfrig tracing/inspection tools. *)

    and Override =
        { AllowOverride: bool
          Overridden: bool }

    let private overridable =
        { AllowOverride = true
          Overridden = false }

    let private nonOverridable =
        { AllowOverride = false
          Overridden = false }

    (* Actions
       
       Action nodes execute some kind of "side-effecting" logical action
       (i.e. in response to a DELETE, POST, etc. method which is generally
       non-idempotent). They will generally need overriding if the resource
       is going to support the associated method. *)

    let private actionDefinitions =

       // Id                                Next
       // ---------------------------------------------------------------------------------------------------------------------------------

        [ A.Delete,                         D.Deleted 
          A.Patch,                          D.RespondWithEntity
          A.Post,                           D.PostRedirect
          A.Put,                            D.Created ]

    let private actions =
        actionDefinitions
        |> List.map (fun (id, next) ->
               Action { Id = id
                        Override = overridable
                        Action = action
                        Next = next })

    (* Decisions (Public)
        
       Decision nodes are (or should be) side effect free and represent some
       choice to be made (depending generally on the form of the request). The
       decision returns a bool, which is then used to select which node to
       invoke next.
       
       Public decisions may be overridden by the resource programmer
       using declarative machine monad syntax. *)

    let private decisionDefinitionsPublic =

       // Id                                Default                         True                               False
       // ---------------------------------------------------------------------------------------------------------------------------------

        [ D.Allowed,                        true,                           (D.ContentTypeValid,                O.PreForbidden)
          D.Authorized,                     true,                           (D.Allowed,                         O.PreUnauthorized)
          D.AllowPostToGone,                false,                          (A.Post,                            O.PreGone)
          D.AllowPostToMissing,             true,                           (A.Post,                            O.PreNotFound)
          D.AllowPutToMissing,              true,                           (D.Conflicts,                       O.PreNotImplemented)
          D.Conflicts,                      false,                          (O.PreConflict,                     A.Put)
          D.ContentTypeKnown,               true,                           (D.EntityLengthValid,               O.PreUnsupportedMediaType)
          D.ContentTypeValid,               true,                           (D.ContentTypeKnown,                O.PreNotImplemented)
          D.Created,                        true,                           (O.PreCreated,                      D.RespondWithEntity)
          D.Deleted,                        true,                           (D.RespondWithEntity,               O.PreAccepted)
          D.EntityLengthValid,              true,                           (D.MethodOptions,                   O.PreRequestEntityTooLarge)
          D.Existed,                        false,                          (D.MovedPermanently,                D.MethodPostToMissing)
          D.Exists,                         true,                           (D.IfMatchRequested,                D.IfMatchExistsForMissing)
          D.Malformed,                      false,                          (O.PreMalformed,                    D.Authorized)
          D.MovedPermanently,               false,                          (O.PreMovedPermanently,             D.MovedTemporarily)
          D.MovedTemporarily,               false,                          (O.PreMovedTemporarily,             D.MethodPostToGone)
          D.MultipleRepresentations,        false,                          (O.PreMultipleRepresentations,      O.PreOK)
          D.PostRedirect,                   false,                          (O.PreSeeOther,                     D.Created)
          D.Processable,                    true,                           (D.Exists,                          O.PreUnprocessableEntity)
          D.PutToDifferentUri,              false,                          (O.PreMovedPermanently,             D.AllowPutToMissing)
          D.RespondWithEntity,              true,                           (D.MultipleRepresentations,         O.PreNoContent)
          D.ServiceAvailable,               true,                           (D.MethodKnown,                     O.PreServiceUnavailable)
          D.UriTooLong,                     false,                          (O.PreUriTooLong,                   D.MethodSupported) ]

    let private decisionsPublic =
        decisionDefinitionsPublic
        |> List.map (fun (id, def, (t, f)) ->
              Decision { Id = id
                         Override = overridable
                         Decision = decision def
                         True = t
                         False = f })

    (* Decisions (Internal)
        
       Decision nodes are (or should be) side effect free and represent some
       choice to be made (depending generally on the form of the request). The
       decision returns a bool, which is then used to select which node to
       invoke next.
       
       Internal decisions cannot be overridden. *)

    let private decisionDefinitionsInternal =

       // Id                                Decision                        True                                False
       // ---------------------------------------------------------------------------------------------------------------------------------

        [ D.CharsetNegotiable,              Charset.negotiable,             (D.EncodingRequested,               O.PreNotAcceptable)
          D.CharsetRequested,               Charset.requested,              (D.CharsetNegotiable,               D.EncodingRequested)
          D.EncodingNegotiable,             Encoding.negotiable,            (D.Processable,                     O.PreNotAcceptable)
          D.EncodingRequested,              Encoding.requested,             (D.EncodingNegotiable,              D.Processable)
          D.IfMatchAny,                     IfMatch.any,                    (D.IfUnmodifiedSinceRequested,      D.ETagMatchesIf)
          D.IfMatchExistsForMissing,        IfMatch.requested,              (O.PrePreconditionFailed,           D.MethodPut)
          D.IfMatchRequested,               IfMatch.requested,              (D.IfMatchAny,                      D.IfUnmodifiedSinceRequested)
          D.IfModifiedSinceModified,        IfModifiedSince.modified,       (D.MethodDelete,                    O.PreNotModified)
          D.IfModifiedSinceRequested,       IfModifiedSince.requested,      (D.IfModifiedSinceValid,            D.MethodDelete)
          D.IfModifiedSinceValid,           IfModifiedSince.valid,          (D.IfModifiedSinceModified,         D.MethodDelete)
          D.IfNoneMatchAny,                 IfNoneMatch.any,                (D.MethodGetOrHead,                 D.ETagMatchesIfNone)
          D.IfNoneMatchRequested,           IfNoneMatch.requested,          (D.IfNoneMatchAny,                  D.IfModifiedSinceRequested)
          D.IfUnmodifiedSinceModified,      IfUnmodifiedSince.modified,     (D.IfNoneMatchRequested,            O.PrePreconditionFailed)
          D.IfUnmodifiedSinceRequested,     IfUnmodifiedSince.requested,    (D.IfUnmodifiedSinceValid,          D.IfNoneMatchRequested)
          D.IfUnmodifiedSinceValid,         IfUnmodifiedSince.valid,        (D.IfUnmodifiedSinceModified,       D.IfNoneMatchRequested)
          D.LanguageNegotiable,             Language.negotiable,            (D.CharsetRequested,                O.PreNotAcceptable)
          D.LanguageRequested,              Language.requested,             (D.LanguageNegotiable,              D.CharsetRequested)
          D.MediaTypeNegotiable,            MediaType.negotiable,           (D.LanguageRequested,               O.PreNotAcceptable)
          D.MediaTypeRequested,             MediaType.requested,            (D.MediaTypeNegotiable,             D.LanguageRequested)
          D.MethodDelete,                   Method.delete,                  (A.Delete,                          D.MethodPatch)
          D.MethodGetOrHead,                Method.getOrHead,               (O.PreNotModified,                  O.PrePreconditionFailed)
          D.MethodKnown,                    Method.known,                   (D.UriTooLong,                      O.PreUnknownMethod)
          D.MethodOptions,                  Method.options,                 (O.PreOptions,                      D.MediaTypeRequested)
          D.MethodPatch,                    Method.patch,                   (A.Patch,                           D.MethodPostToExisting)
          D.MethodPostToExisting,           Method.post,                    (A.Post,                            D.MethodPutToExisting)
          D.MethodPostToGone,               Method.post,                    (D.AllowPostToGone,                 O.PreGone)
          D.MethodPostToMissing,            Method.post,                    (D.AllowPostToMissing,              O.PreNotFound)
          D.MethodPut,                      Method.put,                     (D.PutToDifferentUri,               D.Existed)
          D.MethodPutToExisting,            Method.put,                     (D.Conflicts,                       D.MultipleRepresentations)
          D.MethodSupported,                Method.supported,               (D.Malformed,                       O.PreMethodNotAllowed)





          D.ETagMatchesIf,                  ifETagMatchesIf,                (D.IfUnmodifiedSinceRequested,      O.PrePreconditionFailed)
          D.ETagMatchesIfNone,              ifETagMatchesIfNone,            (D.MethodGetOrHead,                 D.IfModifiedSinceRequested) ]

    let private decisionsInternal =
        decisionDefinitionsInternal
        |> List.map (fun (id, decision, (t, f)) ->
              Decision { Id = id
                         Override = nonOverridable
                         Decision = decision
                         True = t
                         False = f })

    (* Handlers
       
       Handler nodes represent the function which will return some response
       to the client. They are responsible for returning data in an appropriate
       form to Dyfrig.Machine to be sent as part of the response. They always
       represent the final node in a traversal of the execution graph,
       so do not include any form of "next" node data. *)

    let private handlerDefinitions =

       // Id
       // -------------------------------------------------------------------------------------------------------------

        [ H.OK
          H.Options
          H.Created
          H.Accepted
          H.NoContent
          H.MovedPermanently
          H.SeeOther
          H.NotModified
          H.MovedTemporarily
          H.MultipleRepresentations
          H.Malformed
          H.Unauthorized
          H.Forbidden
          H.NotFound
          H.MethodNotAllowed
          H.NotAcceptable
          H.Conflict
          H.Gone
          H.PreconditionFailed
          H.RequestEntityTooLarge
          H.UriTooLong
          H.UnsupportedMediaType
          H.UnprocessableEntity
          H.Exception
          H.NotImplemented
          H.UnknownMethod
          H.ServiceUnavailable ]

    let private handlers =
        handlerDefinitions 
        |> List.map (fun id ->
              Handler { Id = id
                        Override = overridable
                        Handler = handler })

    (* Operations
        
       Operation nodes represent some consistent action (such as setting headers
       or similar which must take place as part of the execution but does not need
       to be overridden as it will always apply. They are most commonly seen before
       Handler nodes, to make sure that correct header values are set (though the
       handler could override them). Operation nodes cannot be user overridden. *)

    let private operationDefinitions =

       // Id                                Operation                                                           Next
       // ---------------------------------------------------------------------------------------------------------------------------------

        [ O.PreOK,                          (operation 200 "OK"),                                               H.OK
          O.PreOptions,                     (options),                                                          H.Options
          O.PreCreated,                     (operation 201 "Created"),                                          H.Created
          O.PreAccepted,                    (operation 202 "Accepted"),                                         H.Accepted
          O.PreNoContent,                   (operation 204 "No Content"),                                       H.NoContent
          O.PreMovedPermanently,            (operation 301 "Moved Permanently"),                                H.MovedPermanently
          O.PreSeeOther,                    (operation 303 "See Other"),                                        H.SeeOther
          O.PreNotModified,                 (operation 304 "Not Modified"),                                     H.NotModified
          O.PreMovedTemporarily,            (operation 307 "Moved Temporarily"),                                H.MovedTemporarily
          O.PreMultipleRepresentations,     (operation 310 "Multiple Representations"),                         H.MultipleRepresentations
          O.PreMalformed,                   (operation 400 "Bad Request"),                                      H.Malformed
          O.PreUnauthorized,                (operation 401 "Unauthorized"),                                     H.Unauthorized
          O.PreForbidden,                   (operation 403 "Forbidden"),                                        H.Forbidden
          O.PreNotFound,                    (operation 404 "Not Found"),                                        H.NotFound
          O.PreMethodNotAllowed,            (operation 405 "Method Not Allowed"),                               H.MethodNotAllowed
          O.PreNotAcceptable,               (operation 406 "Not Acceptable"),                                   H.NotAcceptable
          O.PreConflict,                    (operation 409 "Conflict"),                                         H.Conflict
          O.PreGone,                        (operation 410 "Gone"),                                             H.Gone
          O.PrePreconditionFailed,          (operation 412 "Precondition Failed"),                              H.PreconditionFailed
          O.PreRequestEntityTooLarge,       (operation 413 "Request Entity Too Large"),                         H.RequestEntityTooLarge
          O.PreUriTooLong,                  (operation 414 "URI Too Long"),                                     H.UriTooLong
          O.PreUnsupportedMediaType,        (operation 415 "Unsupported Media Type"),                           H.UnsupportedMediaType
          O.PreUnprocessableEntity,         (operation 422 "Unprocessable Entity"),                             H.UnprocessableEntity
          O.PreException,                   (operation 500 "Internal Server Error"),                            H.Exception
          O.PreNotImplemented,              (operation 501 "Not Implemented"),                                  H.NotImplemented
          O.PreUnknownMethod,               (operation 501 "Unknown Method"),                                   H.UnknownMethod
          O.PreServiceUnavailable,          (operation 503 "Service Unavailable"),                              H.ServiceUnavailable ] 
        
    let private operations =
        operationDefinitions
        |> List.map (fun (id, operation, next) -> 
               Operation { Id = id
                           Operation = operation
                           Next = next })

    let private nodes =
          actions
        @ decisionsPublic
        @ decisionsInternal
        @ handlers
        @ operations

    let construct (definition: MachineDefinition) =
        nodes
        |> List.map (fun n ->
            match n with
            | Action x ->
                x.Id,
                match x.Override.AllowOverride, getPL (actionPLens x.Id) definition with
                | true, Some action -> 
                    Action { x with Action = action
                                    Override = { x.Override with Overridden = true } }
                | _ -> n
            | Decision x -> 
                x.Id,
                match x.Override.AllowOverride, getPL (decisionPLens x.Id) definition with
                | true, Some decision -> 
                    Decision { x with Decision = decision
                                      Override = { x.Override with Overridden = true } }
                | _ -> n
            | Handler x -> 
                x.Id,
                match x.Override.AllowOverride, getPL (handlerPLens x.Id) definition with
                | true, Some handler -> 
                    Handler { x with Handler = handler
                                     Override = { x.Override with Overridden = true } }
                | _ -> n
            | Operation x ->
                x.Id, n)
        |> Map.ofList

    let execute (graph: Graph) =
        let rec traverse from =
            owin {
                match Map.find from graph with
                | Action action ->
                    do! action.Action
                    printfn "action: %s (overridden? %b)" action.Id action.Override.Overridden
                    return! traverse action.Next
                | Decision decision ->
                    let! p = decision.Decision
                    let next = p |> function | true -> decision.True | _ -> decision.False
                    printfn "decision: %s = %b (overridden? %b)" from p decision.Override.Overridden
                    return! traverse next
                | Handler handler ->
                    printfn "handler: %s (overridden? %b)" handler.Id handler.Override.Overridden
                    return handler.Handler
                | Operation operation ->
                    do! operation.Operation
                    printfn "operation: %s" operation.Id
                    return! traverse operation.Next }

        traverse D.ServiceAvailable


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

    let private write (req: MachineRepresentationRequest) (res: MachineRepresentationResponse) =
        owin {
            match Option.getOrElseOptionF (fun () -> List.tryFind (fun _ -> true) req.Languages) res.Language with
            | Some language -> do! setPLM (Response.Headers.contentLanguage) "Language!"
            | _ -> ()

            match Option.getOrElseOptionF (fun () -> List.tryFind (fun _ -> true) req.MediaTypes) res.MediaType with
            | Some mediaType -> do! setPLM (Response.Headers.contentType) "MediaType!"
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
    
    let reifyMachine (machine: MachineMonad) : Pipeline =
        let _, definition = machine Map.empty
        let graph = construct definition

        owin {
            do! setPLM definitionPLens definition
            do! execute graph >>= invoke

            return Halt }
