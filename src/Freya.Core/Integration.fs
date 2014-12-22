[<AutoOpen>]
module Freya.Core.Integration

open System
open System.Threading.Tasks

(* OWIN Types *)

type OwinEnvironment =
    FreyaEnvironment

type OwinApp = 
    OwinEnvironment -> Async<unit>

type OwinAppFunc = 
    Func<OwinEnvironment, Task>

(* OWIN Conversion *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

    [<CompiledName ("FromFreya")>]
    let fromFreya (freya: Freya<_>) =
        OwinAppFunc (fun e -> 
            Async.StartAsTask (async { 
                do! freya { Environment = e
                            Meta = { Memos = Map.empty } } |> Async.Ignore }) :> Task)