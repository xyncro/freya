[<AutoOpen>]
module Freya.Machine.Extension

// TODO: Proper error handling
// TODO: Refactor

let private mapExtension e =
    Dependency (DependencyNode e.Name,
                Set.map DependencyNode e.Dependencies)

let private analyzeExtensions =
       Set.map mapExtension
    >> createDependencyGraph
    >> analyzeDependencyGraph

let private findExtension extensions (DependencyNode x) =
    List.find (fun e -> e.Name = x) (Set.toList extensions)

let applyExtensions (extensions: Set<MachineExtension>) graph =
    match analyzeExtensions extensions with
    | Ordered order ->
        let orderedExtensions = List.map (findExtension extensions) order
        
        List.fold (fun g e ->
            match applyOperations e.DefinitionGraphOperations g with
            | Graph graph -> graph
            | Error e -> failwith e) graph orderedExtensions

    | Cyclic ->
        failwith "cyclic dependencies detected"