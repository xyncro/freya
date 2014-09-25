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
        Map<string, MachineOverride>

    and MachineOverride =
        | Action of MachineAction
        | Configuration of obj
        | Decision of MachineDecision
        | Handler of MachineHandler

    and MachineAction = 
        OwinMonad<unit>

    and MachineDecision = 
        OwinMonad<bool>

    and MachineHandler = 
        OwinMonad<byte []>


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

    let actionPLens k =
             mapPLens k
        <??> ((function | Action x -> Some x | _ -> None), 
              (fun x -> Action x))

    
    let configurationPLens<'T> k =
             mapPLens k
        <??> ((function | Configuration x -> Some x | _ -> None), 
              (fun x -> Configuration x))
        <?-> boxIso<'T>
        
    let decisionPLens k =
             mapPLens k
        <??> ((function | Decision x -> Some x | _ -> None), 
              (fun x -> Decision x))

    let handlerPLens k =
             mapPLens k
        <??> ((function | Handler x -> Some x | _ -> None), 
              (fun x -> Handler x))

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
            Option.getOrElse d <!> getPLM (definitionLens >-?> configurationPLens<Set<Method>> k)

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
          Override: Override
          Action: MachineAction
          OnNext: string }
    and DecisionNode =
        { Metadata: Metadata
          Override: Override
          Decision: MachineDecision
          OnTrue: string
          OnFalse: string }

    and HandlerNode =
        { Metadata: Metadata
          Override: Override
          Handler: MachineHandler }

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
                     { Name = Actions.Delete
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = defaultAction
                   OnNext = Decisions.Deleted }

          Action { Metadata =
                     { Name = Actions.Patch
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = defaultAction
                   OnNext = Decisions.RespondWithEntity }

          Action { Metadata =
                     { Name = Actions.Post
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = defaultAction
                   OnNext = Decisions.PostRedirect }

          Action { Metadata =
                     { Name = Actions.Put
                       Description = None }
                   Override =
                     { AllowOverride = true
                       Overridden = false }
                   Action = defaultAction
                   OnNext = Decisions.Created }

          // Decisions (Allow Override = false)

          Decision { Metadata =
                       { Name = Decisions.AcceptCharsetExists
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifAcceptCharsetExists
                     OnTrue = Decisions.CharsetAvailable
                     OnFalse = Decisions.AcceptEncodingExists }

          Decision { Metadata =
                       { Name = Decisions.AcceptEncodingExists
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifAcceptEncodingExists
                     OnTrue = Decisions.EncodingAvailable
                     OnFalse = Decisions.Processable }

          Decision { Metadata =
                       { Name = Decisions.AcceptExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifAcceptExists
                     OnTrue = Decisions.MediaTypeAvailable
                     OnFalse = Decisions.AcceptLanguageExists }

          Decision { Metadata =
                       { Name = Decisions.AcceptLanguageExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifAcceptLanguageExists
                     OnTrue = Decisions.LanguageAvailable
                     OnFalse = Decisions.AcceptCharsetExists }

          Decision { Metadata =
                       { Name = Decisions.IfMatchExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMatchExists
                     OnTrue = Decisions.IfMatchStar
                     OnFalse = Decisions.IfUnmodifiedSinceExists }

          Decision { Metadata =
                       { Name = Decisions.IfMatchStar 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMatchStar
                     OnTrue = Decisions.IfUnmodifiedSinceExists
                     OnFalse = Decisions.ETagMatchesIf }

          Decision { Metadata =
                       { Name = Decisions.IfMatchStarExistsForMissing 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMatchExists
                     OnTrue = Handlers.PreconditionFailed
                     OnFalse = Decisions.MethodPut }

          Decision { Metadata =
                       { Name = Decisions.IfModifiedSinceExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifModifiedSinceExists
                     OnTrue = Decisions.IfModifiedSinceValidDate
                     OnFalse = Decisions.MethodDelete }

          Decision { Metadata =
                       { Name = Decisions.IfModifiedSinceValidDate 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifModifiedSinceValidDate
                     OnTrue = Decisions.ModifiedSince
                     OnFalse = Decisions.MethodDelete }

          Decision { Metadata =
                       { Name = Decisions.IfNoneMatch 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifNoneMatch
                     OnTrue = Handlers.NotModified
                     OnFalse = Handlers.PreconditionFailed }

          Decision { Metadata =
                       { Name = Decisions.IfNoneMatchExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifNoneMatchExists
                     OnTrue = Decisions.IfNoneMatchStar
                     OnFalse = Decisions.IfModifiedSinceExists }

          Decision { Metadata =
                       { Name = Decisions.IfNoneMatchStar 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifNoneMatchStar
                     OnTrue = Decisions.IfNoneMatch
                     OnFalse = Decisions.ETagMatchesIfNone }

          Decision { Metadata =
                       { Name = Decisions.IfUnmodifiedSinceExists 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifUnmodifiedSinceExists
                     OnTrue = Decisions.IfUnmodifiedSinceValidDate
                     OnFalse = Decisions.IfNoneMatchExists }

          Decision { Metadata =
                       { Name = Decisions.IfUnmodifiedSinceValidDate 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifUnmodifiedSinceValidDate
                     OnTrue = Decisions.UnmodifiedSince
                     OnFalse = Decisions.IfNoneMatchExists }

          Decision { Metadata =
                       { Name = Decisions.MethodDelete 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodDelete
                     OnTrue = Actions.Delete
                     OnFalse = Decisions.MethodPatch }

          Decision { Metadata =
                       { Name = Decisions.MethodOptions 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodOptions
                     OnTrue = Handlers.Options
                     OnFalse = Decisions.AcceptExists }

          Decision { Metadata =
                       { Name = Decisions.MethodPatch 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodPatch
                     OnTrue = Actions.Patch
                     OnFalse = Decisions.PostToExisting }

          Decision { Metadata =
                       { Name = Decisions.MethodPut 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodPut
                     OnTrue = Decisions.PutToDifferentUri
                     OnFalse = Decisions.Existed }

          Decision { Metadata =
                       { Name = Decisions.PostToGone 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodPost
                     OnTrue = Decisions.CanPostToGone
                     OnFalse = Handlers.Gone }

          Decision { Metadata =
                       { Name = Decisions.PostToExisting 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodPost
                     OnTrue = Actions.Post
                     OnFalse = Decisions.PutToExisting }

          Decision { Metadata =
                       { Name = Decisions.PostToMissing 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodPost
                     OnTrue = Decisions.CanPostToMissing
                     OnFalse = Handlers.NotFound }

          Decision { Metadata =
                       { Name = Decisions.PutToExisting 
                         Description = None }
                     Override =
                       { AllowOverride = false
                         Overridden = false }
                     Decision = ifMethodPost
                     OnTrue = Decisions.Conflict
                     OnFalse = Decisions.MultipleRepresentations }

          // Decisions (Allow Override = true)

          Decision { Metadata =
                       { Name = Decisions.Allowed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.ContentTypeValid
                     OnFalse = Handlers.Forbidden }
                      
          Decision { Metadata =
                       { Name = Decisions.Authorized
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.Allowed
                     OnFalse = Handlers.Unauthorized }
                      
          Decision { Metadata =
                       { Name = Decisions.CanPostToGone
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Actions.Post
                     OnFalse = Handlers.Gone }
                      
          Decision { Metadata =
                       { Name = Decisions.CanPostToMissing
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Actions.Post
                     OnFalse = Handlers.NotFound }
                      
          Decision { Metadata =
                       { Name = Decisions.CanPutToMissing
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.Conflict
                     OnFalse = Handlers.NotImplemented }
                      
          Decision { Metadata =
                       { Name = Decisions.CharsetAvailable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifCharsetAvailable
                     OnTrue = Decisions.AcceptEncodingExists
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.Conflict
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.Conflict
                     OnFalse = Actions.Put }
                      
          Decision { Metadata =
                       { Name = Decisions.ContentTypeKnown
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.ValidEntityLength
                     OnFalse = Handlers.UnsupportedMediaType }
                      
          Decision { Metadata =
                       { Name = Decisions.ContentTypeValid
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.ContentTypeKnown
                     OnFalse = Handlers.NotImplemented }
                      
          Decision { Metadata =
                       { Name = Decisions.Created
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Handlers.Created
                     OnFalse = Decisions.RespondWithEntity }
                      
          Decision { Metadata =
                       { Name = Decisions.Deleted
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.RespondWithEntity
                     OnFalse = Handlers.Accepted }
                      
          Decision { Metadata =
                       { Name = Decisions.EncodingAvailable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifEncodingAvailable
                     OnTrue = Decisions.Processable
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.ETagMatchesIf
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifETagMatchesIf
                     OnTrue = Decisions.IfUnmodifiedSinceExists
                     OnFalse = Handlers.PreconditionFailed }
                      
          Decision { Metadata =
                       { Name = Decisions.ETagMatchesIfNone
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifETagMatchesIfNone
                     OnTrue = Decisions.IfNoneMatch
                     OnFalse = Decisions.IfModifiedSinceExists }
                      
          Decision { Metadata =
                       { Name = Decisions.Existed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Decisions.MovedPermanently
                     OnFalse = Decisions.PostToMissing }
                      
          Decision { Metadata =
                       { Name = Decisions.Exists
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.IfMatchExists
                     OnFalse = Decisions.IfMatchStarExistsForMissing }
                      
          Decision { Metadata =
                       { Name = Decisions.MethodKnown
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifMethodKnown
                     OnTrue = Decisions.UriTooLong
                     OnFalse = Handlers.UnknownMethod }
                      
          Decision { Metadata =
                       { Name = Decisions.LanguageAvailable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifLanguageAvailable
                     OnTrue = Decisions.AcceptCharsetExists
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.Malformed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.Malformed
                     OnFalse = Decisions.Authorized }
                      
          Decision { Metadata =
                       { Name = Decisions.MediaTypeAvailable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifMediaTypeAvailable
                     OnTrue = Decisions.AcceptLanguageExists
                     OnFalse = Handlers.NotAcceptable }
                      
          Decision { Metadata =
                       { Name = Decisions.MethodAllowed
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifMethodAllowed
                     OnTrue = Decisions.Malformed
                     OnFalse = Handlers.MethodNotAllowed }
                      
          Decision { Metadata =
                       { Name = Decisions.ModifiedSince
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifModifiedSince
                     OnTrue = Decisions.MethodDelete
                     OnFalse = Handlers.NotModified }
                      
          Decision { Metadata =
                       { Name = Decisions.MovedPermanently
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.MovedPermanently
                     OnFalse = Decisions.MovedTemporarily }
                      
          Decision { Metadata =
                       { Name = Decisions.MovedTemporarily
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.MovedTemporarily
                     OnFalse = Decisions.PostToGone }
                      
          Decision { Metadata =
                       { Name = Decisions.MultipleRepresentations
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.MultipleRepresentations
                     OnFalse = Handlers.OK }
                      
          Decision { Metadata =
                       { Name = Decisions.PostRedirect
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.SeeOther
                     OnFalse = Decisions.Created }
                      
          Decision { Metadata =
                       { Name = Decisions.Processable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.Exists
                     OnFalse = Handlers.UnprocessableEntity }
                      
          Decision { Metadata =
                       { Name = Decisions.PutToDifferentUri
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.MovedPermanently
                     OnFalse = Decisions.CanPutToMissing }
                      
          Decision { Metadata =
                       { Name = Decisions.RespondWithEntity
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.MultipleRepresentations
                     OnFalse = Handlers.NoContent }
                      
          Decision { Metadata =
                       { Name = Decisions.ServiceAvailable
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.MethodKnown
                     OnFalse = Handlers.ServiceUnavailable }
                      
          Decision { Metadata =
                       { Name = Decisions.UnmodifiedSince
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = ifUnmodifiedSince
                     OnTrue = Handlers.PreconditionFailed
                     OnFalse = Decisions.IfNoneMatchExists }
                      
          Decision { Metadata =
                       { Name = Decisions.UriTooLong
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultFalse
                     OnTrue = Handlers.UriTooLong
                     OnFalse = Decisions.MethodAllowed }

          Decision { Metadata =
                       { Name = Decisions.ValidEntityLength
                         Description = None }
                     Override =
                       { AllowOverride = true
                         Overridden = false }
                     Decision = defaultTrue
                     OnTrue = Decisions.MethodOptions
                     OnFalse = Handlers.RequestEntityTooLarge }

          // Handlers
                     
          Handler { Metadata =
                      { Name = Handlers.OK
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 200 "OK" }
                    
          Handler { Metadata =
                      { Name = Handlers.Options
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultOptions }

          Handler { Metadata =
                      { Name = Handlers.Created
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 201 "Created" }

          Handler { Metadata =
                      { Name = Handlers.Accepted
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 202 "Accepted" }

          Handler { Metadata =
                      { Name = Handlers.NoContent
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 204 "No Content" }

          Handler { Metadata =
                      { Name = Handlers.MovedPermanently
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 301 "Moved Permanently" }

          Handler { Metadata =
                      { Name = Handlers.SeeOther
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 303 "See Other" }

          Handler { Metadata =
                      { Name = Handlers.NotModified
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 304 "Not Modified" }

          Handler { Metadata =
                      { Name = Handlers.MovedTemporarily
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 307 "Moved Temporarily" }

          Handler { Metadata =
                      { Name = Handlers.MultipleRepresentations
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 310 "Multiple Representations" }

          Handler { Metadata =
                      { Name = Handlers.Malformed
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 400 "Bad Request" }

          Handler { Metadata =
                      { Name = Handlers.Unauthorized
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 401 "Unauthorized" }

          Handler { Metadata =
                      { Name = Handlers.Forbidden
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 403 "Forbidden" }

          Handler { Metadata =
                      { Name = Handlers.NotFound
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 404 "Not Found" }

          Handler { Metadata =
                      { Name = Handlers.MethodNotAllowed
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 405 "Method Not Allowed" }

          Handler { Metadata =
                      { Name = Handlers.NotAcceptable
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 406 "Not Acceptable" }

          Handler { Metadata =
                      { Name = Handlers.Conflict
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 409 "Conflict" }

          Handler { Metadata =
                      { Name = Handlers.Gone
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 410 "Gone" }

          Handler { Metadata =
                      { Name = Handlers.PreconditionFailed
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 412 "Precondition Failed" }

          Handler { Metadata =
                      { Name = Handlers.RequestEntityTooLarge
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 413 "Request Entity Too Large" }

          Handler { Metadata =
                      { Name = Handlers.UriTooLong
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 414 "URI Too Long" }

          Handler { Metadata =
                      { Name = Handlers.UnsupportedMediaType
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 415 "Unsupported Media Type" }

          Handler { Metadata =
                      { Name = Handlers.UnprocessableEntity
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 422 "Unprocessable Entity" }

          Handler { Metadata =
                      { Name = Handlers.Exception
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 500 "Internal Server Error" }

          Handler { Metadata =
                      { Name = Handlers.NotImplemented
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 501 "Not Implemented" }

          Handler { Metadata =
                      { Name = Handlers.UnknownMethod
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 501 "Unknown Method" }

          Handler { Metadata =
                      { Name = Handlers.ServiceUnavailable
                        Description = None }
                    Override =
                      { AllowOverride = true
                        Overridden = false }
                    Handler = defaultHandler 503 "Service Unavailable" } ]

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
                | _ -> n)
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
        let definition = machine Map.empty |> snd
        let graph = construct definition

        owin {
            do! setLM definitionLens definition

            let! body = execute graph

            do! setPLM (Response.header "Content-Length") [| string body.Length |]
            do! modLM Response.body (fun x -> x.Write (body, 0, body.Length); x)
        
            return Halt }
