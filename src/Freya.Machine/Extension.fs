[<AutoOpen>]
module internal Freya.Machine.Extension

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

let orderExtensions (extensions: Set<MachineExtension>) =
    match analyzeExtensions extensions with
    | Ordered order -> Choice1Of2 (List.map (findExtension extensions) order)
    | Cyclic -> Choice2Of2 "Cyclic Dependencies"