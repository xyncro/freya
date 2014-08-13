namespace Dyfrig.Machine

open System
open System.Globalization
open FSharpx
open FSharpx.Http
open FSharpx.Lens.Operators
open Dyfrig
open Dyfrig.Machine.Keys
open Dyfrig.Operators


module A = Actions
module C = Config
module D = Decisions
module H = Handlers


[<AutoOpen>]
module Types =

    // Machine Definition

    type MachineDef =
        { Actions: Map<string, Action>
          Configuration: Map<string, obj>
          Decisions: Map<string, Decision> 
          Handlers: Map<string, Handler> }

        static member empty =
            { Actions = Map.empty
              Configuration = Map.empty
              Decisions = Map.empty
              Handlers = Map.empty }

        static member actions =
            { Get = fun x -> x.Actions
              Set = fun a x -> { x with Actions = a } }
        
        static member decisions =
            { Get = fun x -> x.Decisions
              Set = fun d x -> { x with Decisions = d } }

        static member config =
            { Get = fun x -> x.Configuration
              Set = fun c x -> { x with Configuration = c } }

        static member handlers =
            { Get = fun x -> x.Handlers
              Set = fun h x -> { x with Handlers = h } }

    and Action = OwinMonad<unit>
    and Decision = OwinMonad<bool>
    and Handler = OwinMonad<byte []>

    // Machine Execution

    type internal MachineGraph =
        Map<string, MachineNode>

    and MachineNode =
        | Action of Action * string       
        | Decision of Decision * (string * string)
        | Handler of Handler


[<AutoOpen>]
module Monad =

    // Monad Type

    type MachineMonad = 
        MachineDef -> unit * MachineDef

    // Monad Builder

    type MachineMonadBuilder () =

        member x.Return (t) : MachineMonad = 
            tuple2 t

        member x.ReturnFrom f : MachineMonad = 
            f

        member x.Bind (m, k) : MachineMonad = 
            m >> fun (result, resource) -> (k result) resource

        member x.Combine (r1, r2) : MachineMonad = 
            x.Bind (r1, fun () -> r2)

        member internal x.Set (r, lens, value) = 
            x.Bind ((fun res -> (), Lens.set value res lens), fun _ -> x.ReturnFrom r)
    
    // Monad Expression

    let machine = MachineMonadBuilder ()


[<AutoOpen>]
module Lenses =

    let internal action k = MachineDef.actions >>| Lens.forMap k
    let internal config k = MachineDef.config >>| Lens.forMap k
    let internal decision k = MachineDef.decisions >>| Lens.forMap k
    let internal handler k = MachineDef.handlers >>| Lens.forMap k    


    [<RequireQualifiedAccess>]
    module Machine =

        let Definition = key<FrostResourceDef> "frost.resourceDefinition" >>| required 


[<RequireQualifiedAccess>]
module Accept =

    let private best configKey header defaults negotiation =
        owin {
            let! x = (Option.map unbox >> Option.getOrElse defaults) <!> get (Machine.Definition >>| config configKey)
            let! y = (Option.map (String.concat ",")) <!> get (Request.Header header)
            
            match x, y with
            | x, Some y -> return negotiation x y
            | h :: _, _ -> return Some (h, 1.)
            | _ -> return None }

    let Charset = 
        best C.AvailableCharsets "Accept-Charset" [ "UTF-8" ] Conneg.bestCharset @>> "frost.resource.charset"

    let Encoding = 
        best C.AvailableEncodings "Accept-Encoding" [ "identity" ] Conneg.bestEncoding @>> "frost.resource.encoding"

    let Language = 
        best C.AvailableLanguages "Accept-Language" [ "*" ] Conneg.bestLanguage @>> "frost.resource.language"

    let MediaType = 
        best C.AvailableMediaTypes "Accept" [] Conneg.bestMediaType @>> "frost.resource.mediaType"


[<AutoOpen>]
module internal Actions =

    let defaultAction = 
        owin { return () }


[<AutoOpen>]
module internal Decisions =

    let defaultTrue = 
        owin { return true }

    let defaultFalse = 
        owin { return false }

    let headerExists h =
        Option.isSome <!> get (Request.Header h)

    let isMethod m =
        (=) m <!> get Request.Method

    let methodIn c defaults =
        owin {
            let! m = get Request.Method
            let! s = Option.map unbox <!> get (Resource.Definition >>| config c)

            return Set.contains m (s |> Option.getOrElse (Set defaults)) }

    let methodAllowed =
        methodIn C.AllowedMethods [ GET; HEAD ]

    let methodKnown =
        methodIn C.KnownMethods [ DELETE; HEAD; GET; OPTIONS; PATCH; POST; PUT; TRACE ]

    let ifMatchStar =
        (=) (Some [ "*" ]) <!> get (Request.Header "If-Match")

    let ifNoneMatchStar =
        (=) (Some [ "*" ]) <!> get (Request.Header "If-None-Match")

    let tryParseDate d =
        DateTime.TryParse 
            (d, CultureInfo.InvariantCulture.DateTimeFormat, 
                DateTimeStyles.AdjustToUniversal)

    let validDate header =
        owin {
            let! header = Option.get <!> get (Request.Header header)

            match header with
            | h :: _ -> return fst <| tryParseDate h
            | _ -> return false }

    let ifModifiedSinceValidDate =
        validDate "If-Modified-Since"

    let ifUnmodifiedSinceValidDate =
        validDate "If-Unmodified-Since"


[<AutoOpen>]
module internal Handlers =

    let defaultHandler code phrase =
        owin {
            do! Response.StatusCode <-- Some code
            do! Response.ReasonPhrase <-- Some phrase

            return Array.empty }


[<AutoOpen>]
module internal Graph =

    let private actions =
        [ A.Delete, defaultAction, D.Deleted
          A.Patch, defaultAction, D.RespondWithEntity
          A.Post, defaultAction, D.PostRedirect
          A.Put, defaultAction, D.Created ]

    let private handlers =
        [ H.OK, defaultHandler 200 "OK"
          H.Created, defaultHandler 201 "Created"
          H.Options, defaultHandler 201 "Options"
          H.Accepted, defaultHandler 202 "Accepted"
          H.NoContent, defaultHandler 204 "No Content"
          H.MovedPermanently, defaultHandler 301 "Moved Permanently"
          H.SeeOther, defaultHandler 303 "See Other"
          H.NotModified, defaultHandler 304 "Not Modified"
          H.MovedTemporarily, defaultHandler 307 "Moved Temporarily"
          H.MultipleRepresentations, defaultHandler 310 "Multiple Representations"
          H.Malformed, defaultHandler 400 "Bad Request"
          H.Unauthorized, defaultHandler 401 "Unauthorized"
          H.Forbidden, defaultHandler 403 "Forbidden"
          H.NotFound, defaultHandler 404 "Not Found"
          H.MethodNotAllowed, defaultHandler 405 "Method Not Allowed"
          H.NotAcceptable, defaultHandler 406 "Not Acceptable"
          H.Conflict, defaultHandler 409 "Conflict"
          H.Gone, defaultHandler 410 "Gone"
          H.PreconditionFailed, defaultHandler 412 "Precondition Failed"
          H.RequestEntityTooLarge, defaultHandler 413 "Request Entity Too Large"
          H.UriTooLong, defaultHandler 414 "URI Too Long"
          H.UnsupportedMediaType, defaultHandler 415 "Unsupported Media Type"
          H.UnprocessableEntity, defaultHandler 422 "Unprocessable Entity"
          H.Exception, defaultHandler 500 "Internal Server Error"
          H.NotImplemented, defaultHandler 501 "Not Implemented"
          H.UnknownMethod, defaultHandler 501 "Unknown Method"
          H.ServiceUnavailable, defaultHandler 503 "Service Unavailable" ]
    
    let private internalDecisions =
        [ D.AcceptCharsetExists, headerExists "Accept-Charset", (D.CharsetAvailable, D.AcceptEncodingExists)
          D.AcceptEncodingExists, headerExists "Accept-Encoding", (D.EncodingAvailable, D.Processable)
          D.AcceptExists, headerExists "Accept", (D.MediaTypeAvailable, D.AcceptLanguageExists)
          D.AcceptLanguageExists, headerExists "Accept-Language", (D.LanguageAvailable, D.AcceptCharsetExists)
          D.IfMatchExists, headerExists "If-Match", (D.IfMatchStar, D.IfUnmodifiedSinceExists)
          D.IfMatchStar, ifMatchStar, (D.IfUnmodifiedSinceExists, D.ETagMatchesIf)
          D.IfMatchStarExistsForMissing, headerExists "If-Match", (H.PreconditionFailed, D.MethodPut)
          D.IfModifiedSinceExists, headerExists "If-Modified-Since", (D.IfModifiedSinceValidDate, D.MethodDelete)
          D.IfModifiedSinceValidDate, ifModifiedSinceValidDate, (D.ModifiedSince, D.MethodDelete)
          D.IfNoneMatch, defaultTrue, (H.NotModified, H.PreconditionFailed) // replace
          D.IfNoneMatchExists, headerExists "If-None-Match", (D.IfNoneMatchStar, D.IfModifiedSinceExists)
          D.IfNoneMatchStar, ifNoneMatchStar, (D.IfNoneMatch, D.ETagMatchesIfNone)
          D.IfUnmodifiedSinceExists, headerExists "If-Unmodified-Since", (D.IfUnmodifiedSinceValidDate, D.IfNoneMatchExists)
          D.IfUnmodifiedSinceValidDate, ifUnmodifiedSinceValidDate, (D.UnmodifiedSince, D.IfNoneMatchExists)
          D.MethodDelete, isMethod DELETE, (A.Delete, D.MethodPatch)
          D.MethodOptions, isMethod OPTIONS, (H.Options, D.AcceptExists)
          D.MethodPatch, isMethod PATCH, (A.Patch, D.PostToExisting)
          D.MethodPut, isMethod PUT, (D.PutToDifferentUri, D.Existed)
          D.PostToGone, isMethod POST, (D.CanPostToGone, H.Gone)
          D.PostToExisting, isMethod POST, (A.Post, D.PutToExisting)
          D.PostToMissing, isMethod POST, (D.CanPostToMissing, H.NotFound)
          D.PutToExisting, isMethod PUT, (D.Conflict, D.MultipleRepresentations) ]

    let private publicDecisions =
        [ D.Allowed, defaultTrue, (D.ContentTypeValid, H.Forbidden)
          D.Authorized, defaultTrue, (D.Allowed, H.Unauthorized)
          D.CanPostToGone, defaultFalse, (A.Post, H.Gone)
          D.CanPostToMissing, defaultTrue, (A.Post, H.NotFound)
          D.CanPutToMissing, defaultTrue, (D.Conflict, H.NotImplemented)
          D.CharsetAvailable, Option.isSome <!> Accept.Charset, (D.AcceptEncodingExists, H.NotAcceptable)
          D.Conflict, defaultFalse, (H.Conflict, A.Put)
          D.ContentTypeKnown, defaultTrue, (D.ValidEntityLength, H.UnsupportedMediaType)
          D.ContentTypeValid, defaultTrue, (D.ContentTypeKnown, H.NotImplemented)
          D.Created, defaultTrue, (H.Created, D.RespondWithEntity)
          D.Deleted, defaultTrue, (D.RespondWithEntity, H.Accepted)
          D.EncodingAvailable, Option.isSome <!> Accept.Encoding, (D.Processable, H.NotAcceptable)
          D.ETagMatchesIf, defaultTrue, (D.IfUnmodifiedSinceExists, H.PreconditionFailed) // replace
          D.ETagMatchesIfNone, defaultTrue, (D.IfNoneMatch, D.IfModifiedSinceExists) // replace
          D.Existed, defaultFalse, (D.MovedPermanently, D.PostToMissing)
          D.Exists, defaultTrue, (D.IfMatchExists, D.IfMatchStarExistsForMissing)
          D.MethodKnown, methodKnown, (D.UriTooLong, H.UnknownMethod)
          D.LanguageAvailable, Option.isSome <!> Accept.Language, (D.AcceptCharsetExists, H.NotAcceptable)
          D.Malformed, defaultFalse, (H.Malformed, D.Authorized)
          D.MediaTypeAvailable, Option.isSome <!> Accept.MediaType, (D.AcceptLanguageExists, H.NotAcceptable)
          D.MethodAllowed, methodAllowed, (D.Malformed, H.MethodNotAllowed)
          D.ModifiedSince, defaultTrue, (D.MethodDelete, H.NotModified) // replace
          D.MovedPermanently, defaultFalse, (H.MovedPermanently, D.MovedTemporarily)
          D.MovedTemporarily, defaultFalse, (H.MovedTemporarily, D.PostToGone)
          D.MultipleRepresentations, defaultFalse, (H.MultipleRepresentations, H.OK)
          D.PostRedirect, defaultFalse, (H.SeeOther, D.Created)
          D.Processable, defaultTrue, (D.Exists, H.UnprocessableEntity)
          D.PutToDifferentUri, defaultFalse, (H.MovedPermanently, D.CanPutToMissing)
          D.RespondWithEntity, defaultTrue, (D.MultipleRepresentations, H.NoContent)
          D.ServiceAvailable, defaultTrue, (D.MethodKnown, H.ServiceUnavailable)
          D.UnmodifiedSince, defaultTrue, (H.PreconditionFailed, D.IfNoneMatchExists) // replace
          D.UriTooLong, defaultFalse, (H.UriTooLong, D.MethodAllowed) 
          D.ValidEntityLength, defaultTrue, (D.MethodOptions, H.RequestEntityTooLarge) ]

    let private getOrElse l k a r =
        Lens.get r (l k) |> Option.getOrElse a

    let private actionNodesOf x r =
        x |> List.map (fun (n, f, next) -> n, Action (getOrElse action n f r, next))

    let private handlerNodesOf x r =
        x |> List.map (fun (n, h) -> n, Handler (getOrElse handler n h r))

    let private internalDecisionNodesOf x r =
        x |> List.map (fun (n, f, choices) -> n, Decision (f, choices))

    let private publicDecisionNodesOf x r =
        x |> List.map (fun (n, f, choices) -> n, Decision (getOrElse decision n f r, choices))


    let defOf (machine: MachineMonad) =
        machine MachineDef.empty |> snd

    let graphOf resource =
        [ actionNodesOf actions
          handlerNodesOf handlers
          internalDecisionNodesOf internalDecisions
          publicDecisionNodesOf publicDecisions ]
        |> Seq.map (fun f -> f resource)
        |> Seq.concat
        |> Map.ofSeq


[<AutoOpen>]
module internal Execution =

    let execute graph =
        let rec execute node =
            owin {
                match Map.find node graph with
                | Action (f, next) ->
                    do! f
                    //printfn "action: %s" node
                    return! execute next
                | Decision (f, choices) ->
                    let! p = f
                    //printfn "decision: %s = %b" node p
                    return! execute ((p |> function | true -> fst | _ -> snd) choices)
                | Handler f ->
                    //printfn "handler: %s" node
                    return! f }

        execute D.ServiceAvailable
        
        
[<AutoOpen>]
module Functions =         

    let compileMachine machine =
        let def = defOf machine
        let graph = graphOf def
        
        owin {
            do! Machine.Definition <-- def

            let! rep = execute graph

            do! Response.Header "Content-Length" <-- Some [ string rep.Length ]
            do! Response.Body <!- fun x -> x.Write (rep, 0, rep.Length); x

            return true }