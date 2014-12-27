//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//----------------------------------------------------------------------------

[<AutoOpen>]
module internal Freya.Machine.Handlers

open Freya.Core.Operators

(* Handlers

   Handler nodes represent the function which will return some response
   to the client. They are responsible for returning data in an appropriate
   form to Freya.Machine to be sent as part of the response. They always
   represent the final node in a traversal of the execution graph,
   so do not include any form of "next" node data. *)

let private handler _ =
    returnM { Metadata =
                { Charset = None
                  Encodings = None
                  MediaType = None
                  Languages = None }
              Data = Array.empty }

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
      Handlers.NotImplemented
      Handlers.UnknownMethod
      Handlers.ServiceUnavailable ]

let handlers =
    handlerDefinitions 
    |> List.map (fun id ->
            HandlerNode { Id = id
                          Override =
                            { Allow = true
                              Overridden = false }
                          Handler = handler })
