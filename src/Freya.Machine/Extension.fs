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
module internal Freya.Machine.Extension

// TODO: Proper error handling
// TODO: Refactor

let private mapExtension e =
    Dependency (Ref e.Name,
                Set.map Ref e.Dependencies)

let private analyzeExtensions =
       Set.map mapExtension
    >> createDependencyGraph
    >> analyzeDependencyGraph

let private findExtension extensions (Ref x) =
    List.find (fun e -> e.Name = x) (Set.toList extensions)

let order (extensions: Set<FreyaMachineExtension>) =
    match analyzeExtensions extensions with
    | Ordered order -> Choice1Of2 (List.map (findExtension extensions) order)
    | Cyclic -> Choice2Of2 "Cyclic Dependencies"