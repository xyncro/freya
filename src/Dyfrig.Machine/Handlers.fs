[<AutoOpen>]
module internal Dyfrig.Machine.Handlers

open Dyfrig.Core.Operators

(* Handlers
       
   Handler nodes represent the function which will return some response
   to the client. They are responsible for returning data in an appropriate
   form to Dyfrig.Machine to be sent as part of the response. They always
   represent the final node in a traversal of the execution graph,
   so do not include any form of "next" node data. *)

let private handler _ =
    returnM { Charset = None
              Encoding = None
              MediaType = None
              Language = None
              Representation = Array.empty }

let private handlerDefinitions =
    [ Handlers.OK
      Handlers.Options
      Handlers.Created
      Handlers.Accepted
      Handlers.NoContent
      Handlers.MovedPermanently
      Handlers.SeeOther
      Handlers.NotModified
      Handlers.MovedTemporarily
      Handlers.MultipleRepresentations
      Handlers.Malformed
      Handlers.Unauthorized
      Handlers.Forbidden
      Handlers.NotFound
      Handlers.MethodNotAllowed
      Handlers.NotAcceptable
      Handlers.Conflict
      Handlers.Gone
      Handlers.PreconditionFailed
      Handlers.RequestEntityTooLarge
      Handlers.UriTooLong
      Handlers.UnsupportedMediaType
      Handlers.UnprocessableEntity
      Handlers.Exception
      Handlers.NotImplemented
      Handlers.UnknownMethod
      Handlers.ServiceUnavailable ]

let handlers =
    handlerDefinitions 
    |> List.map (fun id ->
            Handler { Id = id
                      Override =
                        { Allow = true
                          Overridden = false }
                      Handler = handler })