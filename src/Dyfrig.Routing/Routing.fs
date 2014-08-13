namespace Dyfrig.Routing

open FSharpx
open FSharpx.Lens.Operators
open Dyfrig
open Dyfrig.Operators


[<AutoOpen>]
module Types =

    type RoutingTrie =
        { Root: RoutingNode }

        static member empty =
            { Root =
                { App = None
                  Children = List.empty
                  Key = ""
                  Recognizer = Ignore "" } }

        static member root =
            { Get = fun x -> x.Root
              Set = fun r x -> { x with Root = r } }

    and RoutingNode =
        { App: OwinMonad<bool> option
          Children: RoutingNode list
          Key: string
          Recognizer: RoutingRecognizer }

        static member app =
            { Get = fun x -> x.App
              Set = fun a x -> { x with App = a } }
         
        static member children =
            { Get = fun x -> x.Children
              Set = fun c x -> { x with Children = c } }

    and RoutingRecognizer =
        | Ignore of string
        | Capture of string

    type RoutingRegistration =
        | Registration of string list * OwinMonad<bool>


[<AutoOpen>]
module Lenses =

    [<RequireQualifiedAccess>]
    module Routing =

        let private opt = Lens.xmap (Option.get) (Some) Lens.id
        
        let Values = key<Map<string, string>> "dyfrig.routingData" >>| opt
        let Value key = Values >>| Lens.forMap key


[<AutoOpen>]
module Monad =

    // Monad Type

    type RoutingMonad = 
        RoutingTrie -> unit * RoutingTrie

    // Monad Builder

    type RoutingMonadBuilder () =

        member x.Return v : RoutingMonad = 
            tuple2 v

        member x.ReturnFrom f : RoutingMonad = 
            f

        member x.Bind (r, k) : RoutingMonad = 
            r >> fun (result, trie) -> (k result) trie

        member x.Combine (r1, r2) : RoutingMonad = 
            x.Bind (r1, fun () -> r2)

        member internal x.Update (r, update) = 
            x.Bind ((fun res -> (), update res), fun _ -> x.ReturnFrom r)

    // Monad Expression

    let routing = RoutingMonadBuilder ()


[<AutoOpen>]
module Functions =

    // Registration

    let private path (p: string) =
        p.Split '/'
        |> List.ofArray
        |> List.filter ((<>) "")

    let private registration p app =
        Registration (path p, app)

    let private recognizer (key: string) =
        match key.[0] with
        | ':' -> Capture (key.Substring (1))
        | _ -> Ignore (key)

    let private node key =
        { App = None
          Children = List.empty
          Key = key
          Recognizer = recognizer key }

    let add p app trie =
        let rec add registration root =
            match registration with
            | Registration (h :: t, app) ->
                match List.tryFindIndex (fun x -> x.Key = h) root.Children with
                | Some i -> 
                    Lens.update 
                        (fun x -> add (Registration (t, app)) x) 
                        (RoutingNode.children >>| Lens.forList i) 
                        root
                | _ ->
                    Lens.update 
                        (fun x -> x @ [ add (Registration (t, app)) (node h) ])
                        (RoutingNode.children) 
                        root
            | Registration (_, app) ->
                Lens.set (Some app) root RoutingNode.app

        Lens.update (add (registration p app)) RoutingTrie.root trie

    // Search

    let recognize r data value =
        match r with
        | Capture x -> true, Map.add x value data
        | Ignore x when x = value -> true, data
        | _ -> false, data

    let search p trie =
        let rec search data path root =
            match path with
            | h :: t -> 
                root.Children
                |> List.tryPick (fun x ->
                      match recognize x.Recognizer data h with
                      | true, map -> Some (x, map)
                      | _ -> None)
                |> fun x ->
                    match x with
                    | Some (child, map) -> search map t child
                    | _ -> None
            | _ -> 
                root.App |> Option.map (fun x -> x, data)

        search Map.empty (path p) trie.Root

    // Compilation

    let compileRoutes (routing: RoutingMonad) =
        let trie = snd << routing <| RoutingTrie.empty

        owin {
            let! path = get Request.Path

            match search path trie with
            | Some (app, data) ->
                do! Routing.Values <-- data
                return! app
            | _ -> 
                return true }