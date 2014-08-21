namespace Dyfrig.Router

open Aether
open Aether.Operators
open Dyfrig
open Dyfrig.Http

type internal Trie =
    { Root: Node }

    static member empty =
        { Root =
            { App = None
              Children = List.empty
              Key = ""
              Recognizer = Ignore "" } }

and internal Node =
    { App: OwinMonad<bool> option
      Children: Node list
      Key: string
      Recognizer: Recognizer }    

and internal Recognizer =
    | Ignore of string
    | Capture of string

and internal Registration =
    | Registration of string list * OwinMonad<bool>

[<AutoOpen>]
module internal Lenses =

    let rootLens =
        (fun x -> x.Root), 
        (fun r x -> { x with Root = r })

    let appPLens : PLens<Node, OwinMonad<bool>> =
        (fun x -> x.App), 
        (fun a x -> { x with App = Some a })
         
    let childrenLens =
        (fun x -> x.Children), 
        (fun c x -> { x with Children = c })

[<AutoOpen>]
module internal Paths =

    let path (p: string) =
        p.Split '/'
        |> List.ofArray
        |> List.filter ((<>) "")

[<AutoOpen>]
module internal Construction =

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

    let private add p app =
        let rec add registration root =
            match registration with
            | Registration (h :: t, app) ->
                match List.tryFindIndex (fun x -> x.Key = h) root.Children with
                | Some i -> modPL (childrenLens >-?> listPLens i) (add (Registration (t, app))) root
                | _ -> modL childrenLens (fun x -> x @ [ add (Registration (t, app)) (node h) ]) root
            | Registration (_, app) ->
                setPL appPLens app root

        modL rootLens (add (registration p app))

    let construct (routes: Routes) =
        List.fold (fun x route -> add route.Path route.App x) Trie.empty routes

[<AutoOpen>]
module internal Search =

    let private recognize r data value =
        match r with
        | Capture x -> true, Map.add x value data
        | Ignore x when x = value -> true, data
        | _ -> false, data

    let search p trie =
        let rec traverse data path root =
            match path with
            | h :: t -> 
                root.Children
                |> List.tryPick (fun x ->
                      match recognize x.Recognizer data h with
                      | true, map -> Some (x, map)
                      | _ -> None)
                |> fun x ->
                    match x with
                    | Some (child, map) -> traverse map t child
                    | _ -> None
            | _ -> 
                root.App |> Option.map (fun x -> x, data)

        traverse Map.empty (path p) trie.Root

[<AutoOpen>]
module Compilation =

    let compileRouter (router: Router) =
        let routes = router List.empty |> snd
        let trie = construct routes

        owin {
            let! path = getLM Request.Path

            match search path trie with
            | Some (app, data) ->
                do! setLM Routing.Values data
                return! app
            | _ -> 
                return true }
