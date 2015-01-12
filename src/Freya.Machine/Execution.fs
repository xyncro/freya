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

let private unary (x: CompilationUnaryNode) =
    freya {
        do! x.Unary

        return x.Next }

let private binary (x: CompilationBinaryNode) =
    freya {
        let! result = x.Binary

        return x.Choices.[result] }

let execute (map: CompilationMap) =
    let rec eval ref =
        freya {
            match ref, Map.find ref map with
            | Finish, _ -> return ()
            | _, Unary x -> return! unary x >>= eval
            | _, Binary x -> return! binary x >>= eval }

    eval Start