module Freya.Router.New

open Freya.Types.Uri.Template
open Hekate

(* Types *)

type RoutingGraph =
    Graph<RoutingKey, RoutingNode, RoutingEdge>

and RoutingKey =
    | RoutingKey of string

    override x.ToString () =
        x |> function | RoutingKey key -> sprintf "Key \"%s\"" key

and RoutingNode =
    | RoutingNode of int option

    override x.ToString () =
        x |> function | RoutingNode (Some i) -> sprintf "Node %i" i
                      | _ -> sprintf "Node _"

and RoutingEdge =
    | RoutingEdge of int * UriTemplatePart

    override x.ToString () =
        x |> function | RoutingEdge (i, p) -> 
                            sprintf "%i: Edge \"%s\"" i (p.ToString ())

(* Defaults *)

let private defaultRoutingGraphKey =
    RoutingKey ""

let private defaultRoutingGraphRoot =
    defaultRoutingGraphKey, RoutingNode None

let private defaultRoutingGraph : RoutingGraph =
    Graph.create [ defaultRoutingGraphRoot ] []

(* Construction *)

let rec private add current (graph: RoutingGraph) remaining =
    match remaining with
    | UriTemplate ([]), _ ->
        graph
    | UriTemplate (p :: ps), i ->
        let index =
            graph
            |> Graph.outwardDegree (RoutingKey current)
            |> Option.get

        let next =
            current + UriTemplatePart.Format p

        let value =
            match ps.Length with
            | 0 -> Some i
            | _ -> None
                    
        let graph =
            graph
            |> Graph.addNode (RoutingKey next, RoutingNode value)
            |> Graph.addEdge (RoutingKey current, RoutingKey next, RoutingEdge (index, p))

        add next graph (UriTemplate ps, i)

let create =
    List.fold (add "") defaultRoutingGraph

(* Traversal *)

type TraversalState =
    | TraversalState of TraversalPoint list

and TraversalPoint =
    | TraversalPoint of RoutingKey * int * string

// Shifts

let up =
    function | _ :: TraversalPoint (k, i, p) :: ps -> TraversalState (TraversalPoint (k, i + 1, p) :: ps)
             | _ -> TraversalState []

let down k p =
    function | ps -> TraversalState (TraversalPoint (k, 0, p) :: ps)

let right =
    function | TraversalPoint (k, i, p) :: ps -> TraversalState (TraversalPoint (k, i + 1, p) :: ps)
             | _ -> TraversalState []

let route (graph: RoutingGraph) (path: string) =

    let rec trav (TraversalState points) =
        match points with
        | [] ->
            printfn "top"
            false
        | TraversalPoint (_, _, p) :: _ when p = "" ->
            true
        |  TraversalPoint (k, i, p) :: _ ->
            let next =
                graph
                |> Graph.successors k
                |> Option.bind (
                    List.tryFind (
                        function | (_, RoutingEdge (i', _)) when i' = i -> true
                                 | _ -> false))

            match next with
            | Some (k', RoutingEdge (_, part)) ->
                match part.Match p with
                | _, Some p' ->
                    printfn "matched %A with %s giving %s" part p p'
                    trav (down k' p' points)
                | _ ->
                    printfn "failed to match %A with %s" part p
                    trav (right points)
            | _ ->
                printfn "next not found"
                trav (up points)





    let state =
        TraversalState [ TraversalPoint (defaultRoutingGraphKey, 0, path) ]

    trav state