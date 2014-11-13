[<AutoOpen>]
module Freya.Core.Integration

open System
open System.Collections.Generic
open System.Threading.Tasks

(* OWIN Types *)

type OwinEnvironment =
    IDictionary<string, obj>

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
        OwinAppFunc (fun e -> Async.StartAsTask (async { do! freya e |> Async.Ignore }) :> Task)