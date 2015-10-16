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
//
//----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module internal Freya.Machine.Compilation

open Aether
open Aether.Operators
open Hekate

(* Types

   Types representing the result of compiling a source graph given a
   specification. This should produce the execution graph, used at runtime,
   and the metadata graph, used to support debugging and introspection.

   The Compilation type supports the fact this operation may fail - or
   at least potentially supports this, as currently the type system is not
   designed to find any failure cases (or allow their possibility). *)

type CompilationGraph =
    | Graph of Graph<FreyaMachineNode, FreyaMachineOperation option, FreyaMachineEdge option>

    static member Graph_ =
        (fun (Graph x) -> x), (fun x -> Graph x)

and MetadataGraph =
    | Metadata of Graph<FreyaMachineNode, FreyaMachineOperationMetadata option, FreyaMachineEdge option>

type CompilationResult =
    | Compilation of CompilationGraph * MetadataGraph
    | Error of string

(* Lenses *)

let private precompilationGraph_ =
        id_
   <--> Precompilation.PrecompilationGraph.Graph_

(* Compilation

   Functions to compile a source graph to a compilation result of
   an execution graph and a metadata graph. *)

let private app f =
    fun _ n -> Option.map f n

let private build config graph =
    (fun graph ->
        let g1 = Graph.mapNodes (app (fun (Compile n) -> n config)) graph
        let g2 = Graph.mapNodes (app (fun (FreyaMachineCompilation.Compiled (o, _)) -> o)) g1
        let g3 = Graph.mapNodes (app (fun (FreyaMachineCompilation.Compiled (_, m)) -> m)) g1

        Graph g2, Metadata g3) (graph ^. precompilationGraph_)

(* Compile *)

let compile config graph =
    Compilation (build config graph)