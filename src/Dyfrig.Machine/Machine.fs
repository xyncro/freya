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
        OwinMonad<byte []>

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

    (* Patterns
        
       Active patterns for discriminating between varying kinds of 
       Override within a Machine Definition. *)

    let internal (|Action|) =
        function | Action x -> Some x 
                 | _ -> None

    let internal (|Configuration|) =
        function | Configuration x -> Some x 
                 | _ -> None
        
    let internal (|Decision|) =
        function | Decision x -> Some x
                 | _ -> None

    let internal (|Handler|) =
        function | Handler x -> Some x
                 | _ -> None

    (* Lenses
        
       Partial lenses (Aether form - see https://github.com/xyncro/aether) 
       to the Machine Definition within an OWIN monad (see Dyfrig.Core),
       and to aspects of the machine definition. *)

    let internal defPLens =
             dictPLens "dyfrig.machine.definition"
        <?-> boxIso<MachineDefinition>

    let internal actionPLens k =
             mapPLens k
        <??> ((|Action|), Action)
    
    let internal configPLens<'T> k =
             mapPLens k
        <??> ((|Configuration|), Configuration)
        <?-> boxIso<'T>
        
    let internal decisionPLens k =
             mapPLens k
        <??> ((|Decision|), Decision)

    let internal handlerPLens k =
             mapPLens k
        <??> ((|Handler|), Handler)  


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
        owin { return () }

    (* Configuration

       Overridable configuration properties. These should be overridden
       according to the resource design. Some of these properties such
       as mediaTypesAvailable should almost always be overridden. *)

    let charsetsAvailable =
        [ NamedCharset "iso-8859-1" ]

    let encodingsAvailable =
        List.empty<NamedEncoding>

    let languagesAvailable =
        List.empty<CultureInfo>

    let mediaTypesAvailable =
        List.empty<ClosedMediaRange>
        
    let methodsAllowed =
        Set.ofList
            [ GET
              HEAD ]

    let methodsKnown =
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
        owin { return decision }

    (* Handlers

       A default handler which returns only an empty response, and which should
       be overridden according to the resource design and the methods, etc.
       handled. *)

    let handler =
        owin { return Array.empty<byte> }

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
module internal Logic =

    [<AutoOpen>]
    module Conditional =                        

        let ifNoneMatch =
            decision true // IMPLEMENT

        let ifETagMatchesIf =
            decision true // IMPLEMENT

        let ifETagMatchesIfNone =
            decision true // IMPLEMENT

        let ifModifiedSince =
            decision true // IMPLEMENT
            
        let ifUnmodifiedSince =
            decision true // IMPLEMENT


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

    (* Actions
       
       Action nodes execute some kind of "side-effecting" logical action
       (i.e. in response to a DELETE, POST, etc. method which is generally
       non-idempotent). They will generally need overriding if the resource
       is going to support the associated method. *)
    
    and ActionNode =
        { Metadata: Metadata
          Override: Override
          Action: MachineAction
          Next: string }

    (* Decisions
        
       Decision nodes are (or should be) side effect free and represent some
       choice to be made (depending generally on the form of the request). The
       decision returns a bool, which is then used to select which node to
       invoke next. *)

    and DecisionNode =
        { Metadata: Metadata
          Override: Override
          Decision: MachineDecision
          True: string
          False: string }

    (* Handlers
       
       Handler nodes represent the function which will return some response
       to the client. They are responsible for returning data in an appropriate
       form to Dyfrig.Machine to be sent as part of the response. They always
       represent the final node in a traversal of the execution graph,
       so do not include any form of "next" node data. *)

    and HandlerNode =
        { Metadata: Metadata
          Override: Override
          Handler: MachineHandler }

    (* Operations
        
       Operation nodes represent some consistent action (such as setting headers
       or similar which must take place as part of the execution but does not need
       to be overridden as it will always apply. They are most commonly seen before
       Handler nodes, to make sure that correct header values are set (though the
       handler could override them). Operation nodes cannot be user overridden. *)

    and OperationNode =
        { Metadata: Metadata
          Operation: MachineOperation
          Next: string }

    (* Metadata & Override
       
       Metadata and Override data is used to be able to provide sensible runtime
       introspection and debugging capabilities,such as integration with future 
       Dyfrig tracing/inspection tools. *)

    and Metadata =
        { Name: string
          Description: string option }

    and Override =
        { AllowOverride: bool
          Overridden: bool }

    let private nodes =
        [ 

          // Actions

          Action { Metadata =
                     { Name = A.Delete
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = action
                   Next = D.ResourceDeleted }

          Action { Metadata =
                     { Name = A.Patch
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = action
                   Next = D.RespondWithEntity }

          Action { Metadata =
                     { Name = A.Post
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = action
                   Next = D.PostRedirect }

          Action { Metadata =
                     { Name = A.Put
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = action
                   Next = D.ResourceCreated }

          // Decisions (Allow Override = false)

          Decision { Metadata =
                       { Name = D.RequestAcceptExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.accept
                     True = D.MediaTypeNegotiable
                     False = D.RequestAcceptLanguageExists }
                     
          Decision { Metadata =
                       { Name = D.RequestAcceptCharsetExists
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.acceptCharset
                     True = D.CharsetNegotiable
                     False = D.RequestAcceptEncodingExists }

          Decision { Metadata =
                       { Name = D.RequestAcceptEncodingExists
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.acceptEncoding
                     True = D.EncodingNegotiable
                     False = D.RequestProcessable }

          Decision { Metadata =
                       { Name = D.RequestAcceptLanguageExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.acceptLanguage
                     True = D.LanguageNegotiable
                     False = D.RequestAcceptCharsetExists }

          Decision { Metadata =
                       { Name = D.RequestIfMatchExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.ifMatch
                     True = D.RequestIfMatchStar
                     False = D.IfUnmodifiedSinceExists }

          Decision { Metadata =
                       { Name = D.RequestIfMatchStar 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) (Some IfMatch.Any) <!> getPLM Request.Headers.ifMatch
                     True = D.IfUnmodifiedSinceExists
                     False = D.ResourceETagMatchesIf }

          Decision { Metadata =
                       { Name = D.RequestIfMatchExistsForMissing 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.ifMatch
                     True = O.PrePreconditionFailed
                     False = D.MethodPut }

          Decision { Metadata =
                       { Name = D.RequestIfModifiedSinceExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.ifModifiedSince
                     True = D.RequestIfModifiedSinceValidDate
                     False = D.MethodDelete }

          Decision { Metadata =
                       { Name = D.RequestIfModifiedSinceValidDate 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = 
                            Option.map ((>) DateTime.UtcNow) >> Option.getOrElse false 
                        <!> getPLM Request.Headers.ifModifiedSince
                     True = D.ResourceModifiedSince
                     False = D.MethodDelete }

          Decision { Metadata =
                       { Name = D.IfNoneMatch 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifNoneMatch
                     True = O.PreNotModified
                     False = O.PrePreconditionFailed }

          Decision { Metadata =
                       { Name = D.IfNoneMatchExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.ifNoneMatch
                     True = D.IfNoneMatchStar
                     False = D.RequestIfModifiedSinceExists }

          Decision { Metadata =
                       { Name = D.IfNoneMatchStar 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) (Some IfNoneMatch.Any) <!> getPLM Request.Headers.ifNoneMatch 
                     True = D.IfNoneMatch
                     False = D.ResourceETagMatchesIfNone }

          Decision { Metadata =
                       { Name = D.IfUnmodifiedSinceExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = Option.isSome <!> getPLM Request.Headers.ifUnmodifiedSince
                     True = D.IfUnmodifiedSinceValidDate
                     False = D.IfNoneMatchExists }

          Decision { Metadata =
                       { Name = D.IfUnmodifiedSinceValidDate 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = 
                            Option.map ((>) DateTime.UtcNow) >> Option.getOrElse false
                        <!> getPLM Request.Headers.ifUnmodifiedSince 
                     True = D.ResourceUnmodifiedSince
                     False = D.IfNoneMatchExists }

          Decision { Metadata =
                       { Name = D.MethodDelete 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) DELETE <!> getLM Request.meth
                     True = A.Delete
                     False = D.MethodPatch }

          Decision { Metadata =
                       { Name = D.MethodOptions 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) OPTIONS <!> getLM Request.meth
                     True = O.PreOptions
                     False = D.RequestAcceptExists }

          Decision { Metadata =
                       { Name = D.MethodPatch 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) PATCH <!> getLM Request.meth
                     True = A.Patch
                     False = D.PostToExisting }

          Decision { Metadata =
                       { Name = D.MethodPut 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) PUT <!> getLM Request.meth
                     True = D.PutToDifferentUri
                     False = D.ResourceExisted }

          Decision { Metadata =
                       { Name = D.PostToGone 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) POST <!> getLM Request.meth
                     True = D.CanPostToGone
                     False = O.PreGone }

          Decision { Metadata =
                       { Name = D.PostToExisting 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) POST <!> getLM Request.meth
                     True = A.Post
                     False = D.PutToExisting }

          Decision { Metadata =
                       { Name = D.PostToMissing 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) POST <!> getLM Request.meth
                     True = D.CanPostToMissing
                     False = O.PreNotFound }

          Decision { Metadata =
                       { Name = D.PutToExisting 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = (=) PUT <!> getLM Request.meth
                     True = D.ResourceConflicts
                     False = D.ResourceHasMultipleRepresentations }

          // Decisions (Allow Override = true)

          Decision { Metadata =
                       { Name = D.RequestAllowed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.RequestContentTypeValid
                     False = O.PreForbidden }
                      
          Decision { Metadata =
                       { Name = D.RequestAuthorized
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.RequestAllowed
                     False = O.PreUnauthorized }
                      
          Decision { Metadata =
                       { Name = D.CanPostToGone
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = A.Post
                     False = O.PreGone }
                      
          Decision { Metadata =
                       { Name = D.CanPostToMissing
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = A.Post
                     False = O.PreNotFound }
                      
          Decision { Metadata =
                       { Name = D.CanPutToMissing
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.ResourceConflicts
                     False = O.PreNotImplemented }
                      
          Decision { Metadata =
                       { Name = D.CharsetNegotiable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }

                     // TODO: Make the decision allow for non-configured charsets,
                     // default should probably be to allow through charsets if charsets
                     // are not defined in the machine definition.
                     // See [http://tools.ietf.org/html/rfc7231#section-5.3.3] for logic
                     // on disregarding field if none available.

                     Decision =
                             (fun a r -> Option.isSome (negotiateAcceptCharset a r))
                         <!> (Option.getOrElse charsetsAvailable <!> getPLM (defPLens >??> configPLens C.CharsetsAvailable))
                         <*> (Option.get <!> getPLM Request.Headers.acceptCharset)
                     True = D.RequestAcceptEncodingExists
                     False = O.PreNotAcceptable }
                      
          Decision { Metadata =
                       { Name = D.ResourceConflicts
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreConflict
                     False = A.Put }
                      
          Decision { Metadata =
                       { Name = D.RequestContentTypeKnown
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.RequestEntityLengthValid
                     False = O.PreUnsupportedMediaType }
                      
          Decision { Metadata =
                       { Name = D.RequestContentTypeValid
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.RequestContentTypeKnown
                     False = O.PreNotImplemented }
                      
          Decision { Metadata =
                       { Name = D.ResourceCreated
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = O.PreCreated
                     False = D.RespondWithEntity }
                      
          Decision { Metadata =
                       { Name = D.ResourceDeleted
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.RespondWithEntity
                     False = O.PreAccepted }
                      
          Decision { Metadata =
                       { Name = D.EncodingNegotiable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision =
                             (fun a r -> Option.isSome (negotiateAcceptEncoding a r))
                         <!> (Option.getOrElse encodingsAvailable <!> getPLM (defPLens >??> configPLens C.EncodingsAvailable))
                         <*> (Option.get <!> getPLM Request.Headers.acceptEncoding)
                     True = D.RequestProcessable
                     False = O.PreNotAcceptable }
                      
          Decision { Metadata =
                       { Name = D.ResourceETagMatchesIf
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifETagMatchesIf
                     True = D.IfUnmodifiedSinceExists
                     False = O.PrePreconditionFailed }
                      
          Decision { Metadata =
                       { Name = D.ResourceETagMatchesIfNone
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifETagMatchesIfNone
                     True = D.IfNoneMatch
                     False = D.RequestIfModifiedSinceExists }
                      
          Decision { Metadata =
                       { Name = D.ResourceExisted
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = D.ResourceMovedPermanently
                     False = D.PostToMissing }
                      
          Decision { Metadata =
                       { Name = D.ResourceExists
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.RequestIfMatchExists
                     False = D.RequestIfMatchExistsForMissing }
                      
          Decision { Metadata =
                       { Name = D.MethodKnown
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = 
                             Set.contains 
                         <!> getLM Request.meth 
                         <*> (Option.getOrElse methodsKnown <!> getPLM (defPLens >??> configPLens C.MethodsKnown))
                     True = D.RequestUriTooLong
                     False = O.PreUnknownMethod }
                      
          Decision { Metadata =
                       { Name = D.LanguageNegotiable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision =
                             (fun a r -> Option.isSome (negotiateAcceptLanguage a r))
                         <!> (Option.getOrElse languagesAvailable <!> getPLM (defPLens >??> configPLens C.LanguagesAvailable))
                         <*> (Option.get <!> getPLM Request.Headers.acceptLanguage)
                     True = D.RequestAcceptCharsetExists
                     False = O.PreNotAcceptable }
                      
          Decision { Metadata =
                       { Name = D.RequestMalformed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreMalformed
                     False = D.RequestAuthorized }
                      
          Decision { Metadata =
                       { Name = D.MediaTypeNegotiable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision =
                            (fun a r -> Option.isSome (negotiateAccept a r))
                        <!> (Option.getOrElse mediaTypesAvailable <!> getPLM (defPLens >??> configPLens C.MediaTypesAvailable))
                        <*> (Option.get <!> getPLM Request.Headers.accept)
                     True = D.RequestAcceptLanguageExists
                     False = O.PreNotAcceptable }
                      
          Decision { Metadata =
                       { Name = D.MethodAllowed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = 
                             Set.contains 
                         <!> getLM Request.meth 
                         <*> (Option.getOrElse methodsAllowed <!> getPLM (defPLens >??> configPLens C.MethodsAllowed))
                     True = D.RequestMalformed
                     False = O.PreMethodNotAllowed }
                      
          Decision { Metadata =
                       { Name = D.ResourceModifiedSince
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifModifiedSince
                     True = D.MethodDelete
                     False = O.PreNotModified }
                      
          Decision { Metadata =
                       { Name = D.ResourceMovedPermanently
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreMovedPermanently
                     False = D.ResourceMovedTemporarily }
                      
          Decision { Metadata =
                       { Name = D.ResourceMovedTemporarily
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreMovedTemporarily
                     False = D.PostToGone }
                      
          Decision { Metadata =
                       { Name = D.ResourceHasMultipleRepresentations
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreMultipleRepresentations
                     False = O.PreOK }
                      
          Decision { Metadata =
                       { Name = D.PostRedirect
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreSeeOther
                     False = D.ResourceCreated }
                      
          Decision { Metadata =
                       { Name = D.RequestProcessable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.ResourceExists
                     False = O.PreUnprocessableEntity }
                      
          Decision { Metadata =
                       { Name = D.PutToDifferentUri
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreMovedPermanently
                     False = D.CanPutToMissing }
                      
          Decision { Metadata =
                       { Name = D.RespondWithEntity
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.ResourceHasMultipleRepresentations
                     False = O.PreNoContent }
                      
          Decision { Metadata =
                       { Name = D.ServiceAvailable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.MethodKnown
                     False = O.PreServiceUnavailable }
                      
          Decision { Metadata =
                       { Name = D.ResourceUnmodifiedSince
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifUnmodifiedSince
                     True = O.PrePreconditionFailed
                     False = D.IfNoneMatchExists }
                      
          Decision { Metadata =
                       { Name = D.RequestUriTooLong
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision false
                     True = O.PreUriTooLong
                     False = D.MethodAllowed }

          Decision { Metadata =
                       { Name = D.RequestEntityLengthValid
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = decision true
                     True = D.MethodOptions
                     False = O.PreRequestEntityTooLarge }

          // Handlers
                     
          Handler { Metadata =
                      { Name = H.OK
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }
                    
          Handler { Metadata =
                      { Name = H.Options
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Created
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Accepted
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.NoContent
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.MovedPermanently
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.SeeOther
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler  }

          Handler { Metadata =
                      { Name = H.NotModified
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.MovedTemporarily
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.MultipleRepresentations
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Malformed
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Unauthorized
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Forbidden
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.NotFound
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.MethodNotAllowed
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.NotAcceptable
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Conflict
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Gone
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.PreconditionFailed
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.RequestEntityTooLarge
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.UriTooLong
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.UnsupportedMediaType
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.UnprocessableEntity
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.Exception
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.NotImplemented
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.UnknownMethod
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }

          Handler { Metadata =
                      { Name = H.ServiceUnavailable
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = handler }
                    
          // Operations
          
          Operation { Metadata =
                        { Name = O.PreOK
                          Description = None }
                      Operation = operation 200 "OK"
                      Next = H.OK }
                    
          Operation { Metadata =
                       { Name = O.PreOptions
                         Description = None }
                      Operation = options
                      Next = H.Options }

          Operation { Metadata =
                       { Name = O.PreCreated
                         Description = None }
                      Operation = operation 201 "Created"
                      Next = H.Created }

          Operation { Metadata =
                       { Name = O.PreAccepted
                         Description = None }
                      Operation = operation 202 "Accepted"
                      Next = H.Accepted }

          Operation { Metadata =
                       { Name = O.PreNoContent
                         Description = None }
                      Operation = operation 204 "No Content"
                      Next = H.NoContent }

          Operation { Metadata =
                       { Name = O.PreMovedPermanently
                         Description = None }
                      Operation = operation 301 "Moved Permanently"
                      Next = O.PreMovedPermanently }

          Operation { Metadata =
                       { Name = O.PreSeeOther
                         Description = None }
                      Operation = operation 303 "See Other"
                      Next = H.SeeOther }

          Operation { Metadata =
                       { Name = O.PreNotModified
                         Description = None }
                      Operation = operation 304 "Not Modified"
                      Next = H.NotModified }

          Operation { Metadata =
                       { Name = O.PreMovedTemporarily
                         Description = None }
                      Operation = operation 307 "Moved Temporarily"
                      Next = H.MovedTemporarily }

          Operation { Metadata =
                       { Name = O.PreMultipleRepresentations
                         Description = None }
                      Operation = operation 310 "Multiple Representations"
                      Next = H.MultipleRepresentations }

          Operation { Metadata =
                       { Name = O.PreMalformed
                         Description = None }
                      Operation = operation 400 "Bad Request"
                      Next = H.Malformed }

          Operation { Metadata =
                       { Name = O.PreUnauthorized
                         Description = None }
                      Operation = operation 401 "Unauthorized"
                      Next = H.Unauthorized }

          Operation { Metadata =
                       { Name = O.PreForbidden
                         Description = None }
                      Operation = operation 403 "Forbidden"
                      Next = H.Forbidden }

          Operation { Metadata =
                       { Name = O.PreNotFound
                         Description = None }
                      Operation = operation 404 "Not Found"
                      Next = H.NotFound }

          Operation { Metadata =
                       { Name = O.PreMethodNotAllowed
                         Description = None }
                      Operation = operation 405 "Method Not Allowed"
                      Next = H.MethodNotAllowed }

          Operation { Metadata =
                       { Name = O.PreNotAcceptable
                         Description = None }
                      Operation = operation 406 "Not Acceptable"
                      Next = H.NotAcceptable }

          Operation { Metadata =
                       { Name = O.PreConflict
                         Description = None }
                      Operation = operation 409 "Conflict"
                      Next = H.Conflict }

          Operation { Metadata =
                       { Name = O.PreGone
                         Description = None }
                      Operation = operation 410 "Gone"
                      Next = H.Gone }

          Operation { Metadata =
                       { Name = O.PrePreconditionFailed
                         Description = None }
                      Operation = operation 412 "Precondition Failed"
                      Next = H.PreconditionFailed }

          Operation { Metadata =
                       { Name = O.PreRequestEntityTooLarge
                         Description = None }
                      Operation = operation 413 "Request Entity Too Large"
                      Next = H.RequestEntityTooLarge }

          Operation { Metadata =
                       { Name = O.PreUriTooLong
                         Description = None }
                      Operation = operation 414 "URI Too Long"
                      Next = H.UriTooLong }

          Operation { Metadata =
                       { Name = O.PreUnsupportedMediaType
                         Description = None }
                      Operation = operation 415 "Unsupported Media Type"
                      Next = H.UnsupportedMediaType }

          Operation { Metadata =
                       { Name = O.PreUnprocessableEntity
                         Description = None }
                      Operation = operation 422 "Unprocessable Entity"
                      Next = H.UnprocessableEntity }

          Operation { Metadata =
                       { Name = O.PreException
                         Description = None }
                      Operation = operation 500 "Internal Server Error"
                      Next = H.Exception }

          Operation { Metadata =
                       { Name = O.PreNotImplemented
                         Description = None }
                      Operation = operation 501 "Not Implemented"
                      Next = H.NotImplemented }

          Operation { Metadata =
                       { Name = O.PreUnknownMethod
                         Description = None }
                      Operation = operation 501 "Unknown Method"
                      Next = H.UnknownMethod }

          Operation { Metadata =
                       { Name = O.PreServiceUnavailable
                         Description = None }
                      Operation = operation 503 "Service Unavailable"
                      Next = H.ServiceUnavailable } ]

    let construct (definition: MachineDefinition) =
        nodes
        |> List.map (fun n ->
            match n with
            | Action x ->
                x.Metadata.Name,
                match x.Override.AllowOverride, getPL (actionPLens x.Metadata.Name) definition with
                | true, Some a -> Action { x with Action = a; Override = { x.Override with Overridden = true } }
                | _ -> n
            | Decision x -> 
                x.Metadata.Name,
                match x.Override.AllowOverride, getPL (decisionPLens x.Metadata.Name) definition with
                | true, Some d -> Decision { x with Decision = d; Override = { x.Override with Overridden = true } }
                | _ -> n
            | Handler x -> 
                x.Metadata.Name,
                match x.Override.AllowOverride, getPL (handlerPLens x.Metadata.Name) definition with
                | true, Some h -> Handler { x with Handler = h; Override = { x.Override with Overridden = true } }
                | _ -> n
            | Operation x ->
                x.Metadata.Name, n)
        |> Map.ofList

    let execute (graph: Graph) =
        let rec traverse from =
            owin {
                match Map.find from graph with
                | Action action ->
                    do! action.Action
                    printfn "action: %s (overriden? %b)" action.Metadata.Name action.Override.Overridden
                    return! traverse action.Next
                | Decision decision ->
                    let! p = decision.Decision
                    let next = p |> function | true -> decision.True | _ -> decision.False
                    printfn "decision: %s = %b (overriden? %b)" from p decision.Override.Overridden
                    return! traverse next
                | Handler handler ->
                    printfn "handler: %s (overriden? %b)" handler.Metadata.Name handler.Override.Overridden
                    return! handler.Handler
                | Operation operation ->
                    do! operation.Operation
                    printfn "operation: %s" operation.Metadata.Name
                    return! traverse operation.Next }

        traverse D.ServiceAvailable


[<AutoOpen>]
module Reification =
    
    let reifyMachine (machine: MachineMonad) : Pipeline =
        let definition = machine Map.empty |> snd
        let graph = construct definition

        owin {
            do! setPLM defPLens definition

            let! body = execute graph

            do! setPLM Response.Headers.contentLength body.Length
            do! modLM Response.body (fun x -> x.Write (body, 0, body.Length); x)
        
            return Halt }
