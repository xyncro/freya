namespace Dyfrig.Machine

open System
open System.Globalization
open Aether
open Aether.Operators
open Dyfrig
open Dyfrig.Operators
open FSharpx

module A = Actions
module C = Config
module D = Decisions
module H = Handlers

type ExecutionGraph =
    Map<string, ExecutionNode>

and ExecutionNode =
    | Action of Machine.Action * string       
    | Decision of Machine.Decision * (string * string)
    | Handler of Machine.Handler

[<AutoOpen>]
module internal Lenses =

    let definitionLens =
        owinEnvLens "machine.definition"
        >--> isoBoxLens<Definition>

[<AutoOpen>]
module internal Logic =

    let private headerEquals h v =
        Option.map ((=) v) >> Option.getOrElse false <!> getPLM (Request.Header h)

    let private headerExists h =
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

    [<AutoOpen>]
    module Method =

        let private isMethod meth =
            (=) meth <!> getLM Request.Method

        let private getMethods key defaults =
            Option.getOrElse defaults
            <!> getPLM (definitionLens >-?> Definition.configPLens<Set<Method>> key)

        let private isValidMethod key defaults =
            owin {
                let! m = getLM Request.Method
                let! ms = getMethods key defaults

                return Set.contains m ms }

        let ifMethodAllowed =
            isValidMethod C.AllowedMethods defaultAllowedMethods

        let ifMethodKnown =
            isValidMethod C.KnownMethods defaultKnownMethods

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

[<AutoOpen>]
module internal Construction =

    let private maybe l (def: Definition) other =
        getPL l def |> Option.getOrElse other

    let private actions def =
        [ A.Delete,                      defaultAction,              D.Deleted
          A.Patch,                       defaultAction,              D.RespondWithEntity
          A.Post,                        defaultAction,              D.PostRedirect
          A.Put,                         defaultAction,              D.Created ]

        |> List.map (fun (name, action, next) ->
            name, Action (maybe (Definition.actionsPLens name) def action, next))

    let private handlers def =
        [ H.OK,                          defaultHandler 200 "OK"
          H.Created,                     defaultHandler 201 "Created"
          H.Options,                     defaultHandler 201 "Options"
          H.Accepted,                    defaultHandler 202 "Accepted"
          H.NoContent,                   defaultHandler 204 "No Content"
          H.MovedPermanently,            defaultHandler 301 "Moved Permanently"
          H.SeeOther,                    defaultHandler 303 "See Other"
          H.NotModified,                 defaultHandler 304 "Not Modified"
          H.MovedTemporarily,            defaultHandler 307 "Moved Temporarily"
          H.MultipleRepresentations,     defaultHandler 310 "Multiple Representations"
          H.Malformed,                   defaultHandler 400 "Bad Request"
          H.Unauthorized,                defaultHandler 401 "Unauthorized"
          H.Forbidden,                   defaultHandler 403 "Forbidden"
          H.NotFound,                    defaultHandler 404 "Not Found"
          H.MethodNotAllowed,            defaultHandler 405 "Method Not Allowed"
          H.NotAcceptable,               defaultHandler 406 "Not Acceptable"
          H.Conflict,                    defaultHandler 409 "Conflict"
          H.Gone,                        defaultHandler 410 "Gone"
          H.PreconditionFailed,          defaultHandler 412 "Precondition Failed"
          H.RequestEntityTooLarge,       defaultHandler 413 "Request Entity Too Large"
          H.UriTooLong,                  defaultHandler 414 "URI Too Long"
          H.UnsupportedMediaType,        defaultHandler 415 "Unsupported Media Type"
          H.UnprocessableEntity,         defaultHandler 422 "Unprocessable Entity"
          H.Exception,                   defaultHandler 500 "Internal Server Error"
          H.NotImplemented,              defaultHandler 501 "Not Implemented"
          H.UnknownMethod,               defaultHandler 501 "Unknown Method"
          H.ServiceUnavailable,          defaultHandler 503 "Service Unavailable" ]

        |> List.map (fun (name, handler) ->
            name, Handler (maybe (Definition.handlersPLens name) def handler))
    
    let private internalDecisions =
        [ D.AcceptCharsetExists,         ifAcceptCharsetExists,      (D.CharsetAvailable, D.AcceptEncodingExists)
          D.AcceptEncodingExists,        ifAcceptEncodingExists,     (D.EncodingAvailable, D.Processable)
          D.AcceptExists,                ifAcceptExists,             (D.MediaTypeAvailable, D.AcceptLanguageExists)
          D.AcceptLanguageExists,        ifAcceptLanguageExists,     (D.LanguageAvailable, D.AcceptCharsetExists)
          D.IfMatchExists,               ifMatchExists,              (D.IfMatchStar, D.IfUnmodifiedSinceExists)
          D.IfMatchStar,                 ifMatchStar,                (D.IfUnmodifiedSinceExists, D.ETagMatchesIf)
          D.IfMatchStarExistsForMissing, ifMatchExists,              (H.PreconditionFailed, D.MethodPut) // CHECK THIS
          D.IfModifiedSinceExists,       ifModifiedSinceExists,      (D.IfModifiedSinceValidDate, D.MethodDelete)
          D.IfModifiedSinceValidDate,    ifModifiedSinceValidDate,   (D.ModifiedSince, D.MethodDelete)
          D.IfNoneMatch,                 defaultDecision true,       (H.NotModified, H.PreconditionFailed) // replace
          D.IfNoneMatchExists,           ifNoneMatchExists,          (D.IfNoneMatchStar, D.IfModifiedSinceExists)
          D.IfNoneMatchStar,             ifNoneMatchStar,            (D.IfNoneMatch, D.ETagMatchesIfNone)
          D.IfUnmodifiedSinceExists,     ifUnmodifiedSinceExists,    (D.IfUnmodifiedSinceValidDate, D.IfNoneMatchExists)
          D.IfUnmodifiedSinceValidDate,  ifUnmodifiedSinceValidDate, (D.UnmodifiedSince, D.IfNoneMatchExists)
          D.MethodDelete,                ifMethodDelete,             (A.Delete, D.MethodPatch)
          D.MethodOptions,               ifMethodOptions,            (H.Options, D.AcceptExists)
          D.MethodPatch,                 ifMethodPatch,              (A.Patch, D.PostToExisting)
          D.MethodPut,                   ifMethodPut,                (D.PutToDifferentUri, D.Existed)
          D.PostToGone,                  ifMethodPost,               (D.CanPostToGone, H.Gone)
          D.PostToExisting,              ifMethodPost,               (A.Post, D.PutToExisting)
          D.PostToMissing,               ifMethodPost,               (D.CanPostToMissing, H.NotFound)
          D.PutToExisting,               ifMethodPost,               (D.Conflict, D.MultipleRepresentations) ]

        |> List.map (fun (name, decision, next) ->
            name, Decision (decision, next))

    let private publicDecisions =
        [ D.Allowed,                     defaultDecision true,       (D.ContentTypeValid, H.Forbidden)
          D.Authorized,                  defaultDecision true,       (D.Allowed, H.Unauthorized)
          D.CanPostToGone,               defaultDecision false,      (A.Post, H.Gone)
          D.CanPostToMissing,            defaultDecision true,       (A.Post, H.NotFound)
          D.CanPutToMissing,             defaultDecision true,       (D.Conflict, H.NotImplemented)
          D.CharsetAvailable,            Option.isSome <!> Accept.Charset, (D.AcceptEncodingExists, H.NotAcceptable)
          D.Conflict,                    defaultDecision false,      (H.Conflict, A.Put)
          D.ContentTypeKnown,            defaultDecision true,       (D.ValidEntityLength, H.UnsupportedMediaType)
          D.ContentTypeValid,            defaultDecision true,       (D.ContentTypeKnown, H.NotImplemented)
          D.Created,                     defaultDecision true,       (H.Created, D.RespondWithEntity)
          D.Deleted,                     defaultDecision true,       (D.RespondWithEntity, H.Accepted)
          D.EncodingAvailable,           Option.isSome <!> Accept.Encoding, (D.Processable, H.NotAcceptable)
          D.ETagMatchesIf,               defaultDecision true,       (D.IfUnmodifiedSinceExists, H.PreconditionFailed) // replace
          D.ETagMatchesIfNone,           defaultDecision true,       (D.IfNoneMatch, D.IfModifiedSinceExists) // replace
          D.Existed,                     defaultDecision false,      (D.MovedPermanently, D.PostToMissing)
          D.Exists,                      defaultDecision true,       (D.IfMatchExists, D.IfMatchStarExistsForMissing)
          D.MethodKnown,                 ifMethodKnown,              (D.UriTooLong, H.UnknownMethod)
          D.LanguageAvailable,           Option.isSome <!> Accept.Language, (D.AcceptCharsetExists, H.NotAcceptable)
          D.Malformed,                   defaultDecision false,      (H.Malformed, D.Authorized)
          D.MediaTypeAvailable,          Option.isSome <!> Accept.MediaType, (D.AcceptLanguageExists, H.NotAcceptable)
          D.MethodAllowed,               ifMethodAllowed,            (D.Malformed, H.MethodNotAllowed)
          D.ModifiedSince,               defaultDecision true,       (D.MethodDelete, H.NotModified) // replace
          D.MovedPermanently,            defaultDecision false,      (H.MovedPermanently, D.MovedTemporarily)
          D.MovedTemporarily,            defaultDecision false,      (H.MovedTemporarily, D.PostToGone)
          D.MultipleRepresentations,     defaultDecision false,      (H.MultipleRepresentations, H.OK)
          D.PostRedirect,                defaultDecision false,      (H.SeeOther, D.Created)
          D.Processable,                 defaultDecision true,       (D.Exists, H.UnprocessableEntity)
          D.PutToDifferentUri,           defaultDecision false,      (H.MovedPermanently, D.CanPutToMissing)
          D.RespondWithEntity,           defaultDecision true,       (D.MultipleRepresentations, H.NoContent)
          D.ServiceAvailable,            defaultDecision true,       (D.MethodKnown, H.ServiceUnavailable)
          D.UnmodifiedSince,             defaultDecision true,       (H.PreconditionFailed, D.IfNoneMatchExists) // replace
          D.UriTooLong,                  defaultDecision false,      (H.UriTooLong, D.MethodAllowed) 
          D.ValidEntityLength,           defaultDecision true,       (D.MethodOptions, H.RequestEntityTooLarge) ]

    