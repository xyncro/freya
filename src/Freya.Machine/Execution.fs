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

[<AutoOpen>]
module internal Freya.Machine.Execution

open Freya.Core
open Freya.Core.Operators

(* Aliases

   Convenience type aliases when we have some more specific unions
   etc. in scope, in this case clashes between machine level refs
   and compilation refs. *)

type Ref =
    FreyaMachineRef

(* Execution

   Execution of compilation maps, using the Freya computation expression,
   returning a pipeline result of Halt. *)

let private start (x: CompilationStartNode) =
    freya {
        do! addFreyaMachineExecutionRecord Ref.Start

        return x.Next }

let private finish _ =
    freya {
        do! addFreyaMachineExecutionRecord Ref.Finish

        return () }

let private unary ref (x: CompilationUnaryNode) =
    freya {
        do! addFreyaMachineExecutionRecord ref
        do! x.Unary

        return x.Next }

let private binary ref (x: CompilationBinaryNode) =
    freya {
        do! addFreyaMachineExecutionRecord ref
        let! result = x.Binary

        match result with
        | true -> return x.True
        | _ -> return x.False }

let executeCompilation (map: CompilationMap) =
    let rec eval ref =
        freya {
            match ref, Map.tryFind ref map with
            | Ref.Start, Some (Start x) -> return! start x >>= eval
            | Ref.Finish, Some Finish -> return! finish ()
            | ref, Some (Unary x) -> return! unary ref x >>= eval
            | ref, Some (Binary x) -> return! binary ref x >>= eval
            | _ -> failwith "Invalid Compilation" }

    eval Ref.Start