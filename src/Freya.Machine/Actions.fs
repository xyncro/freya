[<AutoOpen>]
module internal Freya.Machine.Actions

open Freya.Core.Operators

(* Actions
       
   Action nodes execute some kind of "side-effecting" logical action
   (i.e. in response to a DELETE, POST, etc. method which is generally
   non-idempotent). They will generally need overriding if the resource
   is going to support the associated method. *)

let private action =
    returnM ()

let private actionDefinitions =
    [ Actions.Delete,                         Decisions.Deleted 
      Actions.Patch,                          Decisions.RespondWithEntity
      Actions.Post,                           Decisions.PostRedirect
      Actions.Put,                            Decisions.Created ]

let actions =
    actionDefinitions
    |> List.map (fun (id, next) ->
            ActionNode { Id = id
                         Override = 
                           { Allow = true
                             Overridden = false }
                         Action = action
                         Next = next })
