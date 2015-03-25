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

let private defaultRoutingGraph : RoutingGraph =
    Graph.create
        [ (RoutingKey "", RoutingNode None) ]
        []

(* Construction *)

let rec private add current (graph: RoutingGraph) remaining =
    match remaining with
    | UriTemplate ([]), i ->
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

let route (graph: RoutingGraph) (path: string) =
    None