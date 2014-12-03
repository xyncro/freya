[<AutoOpen>]
module internal Freya.Machine.Operations

open Freya.Core.Operators
open Freya.Types
open Freya.Types.Http

(* Operations
        
   Operation nodes represent some consistent action (such as setting headers
   or similar which must take place as part of the execution but does not need
   to be overridden as it will always apply. They are most commonly seen before
   Handler nodes, to make sure that correct header values are set (though the
   handler could override them). Operation nodes cannot be user overridden. *)

let private operation statusCode reasonPhrase =
       setPLM Response.statusCode statusCode
    *> setPLM Response.reasonPhrase reasonPhrase

let private options =
       operation 200 "Options"
    *> setPLM (Response.headersKey "Access-Control-Allow-Origin") "*"
    *> setPLM (Response.headersKey "Access-Control-Allow-Headers") "Content-Type"

let private operationDefinitions =
    [ Operations.PreOK,                          (operation 200 "OK"),                          Handlers.OK
      Operations.PreOptions,                     (options),                                     Handlers.Options
      Operations.PreCreated,                     (operation 201 "Created"),                     Handlers.Created
      Operations.PreAccepted,                    (operation 202 "Accepted"),                    Handlers.Accepted
      Operations.PreNoContent,                   (operation 204 "No Content"),                  Handlers.NoContent
      Operations.PreMovedPermanently,            (operation 301 "Moved Permanently"),           Handlers.MovedPermanently
      Operations.PreSeeOther,                    (operation 303 "See Other"),                   Handlers.SeeOther
      Operations.PreNotModified,                 (operation 304 "Not Modified"),                Handlers.NotModified
      Operations.PreMovedTemporarily,            (operation 307 "Moved Temporarily"),           Handlers.MovedTemporarily
      Operations.PreMultipleRepresentations,     (operation 310 "Multiple Representations"),    Handlers.MultipleRepresentations
      Operations.PreMalformed,                   (operation 400 "Bad Request"),                 Handlers.Malformed
      Operations.PreUnauthorized,                (operation 401 "Unauthorized"),                Handlers.Unauthorized
      Operations.PreForbidden,                   (operation 403 "Forbidden"),                   Handlers.Forbidden
      Operations.PreNotFound,                    (operation 404 "Not Found"),                   Handlers.NotFound
      Operations.PreMethodNotAllowed,            (operation 405 "Method Not Allowed"),          Handlers.MethodNotAllowed
      Operations.PreNotAcceptable,               (operation 406 "Not Acceptable"),              Handlers.NotAcceptable
      Operations.PreConflict,                    (operation 409 "Conflict"),                    Handlers.Conflict
      Operations.PreGone,                        (operation 410 "Gone"),                        Handlers.Gone
      Operations.PrePreconditionFailed,          (operation 412 "Precondition Failed"),         Handlers.PreconditionFailed
      Operations.PreRequestEntityTooLarge,       (operation 413 "Request Entity Too Large"),    Handlers.RequestEntityTooLarge
      Operations.PreUriTooLong,                  (operation 414 "URI Too Long"),                Handlers.UriTooLong
      Operations.PreUnsupportedMediaType,        (operation 415 "Unsupported Media Type"),      Handlers.UnsupportedMediaType
      Operations.PreUnprocessableEntity,         (operation 422 "Unprocessable Entity"),        Handlers.UnprocessableEntity
      Operations.PreException,                   (operation 500 "Internal Server Error"),       Handlers.Exception
      Operations.PreNotImplemented,              (operation 501 "Not Implemented"),             Handlers.NotImplemented
      Operations.PreUnknownMethod,               (operation 501 "Unknown Method"),              Handlers.UnknownMethod
      Operations.PreServiceUnavailable,          (operation 503 "Service Unavailable"),         Handlers.ServiceUnavailable ] 
        
let operations =
    operationDefinitions
    |> List.map (fun (id, operation, next) -> 
            OperationNode { Id = id
                            Operation = operation
                            Next = next })
