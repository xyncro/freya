[<AutoOpen>]
module internal Dyfrig.Machine.Inspection

open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Inspector

(* Literals *)

let [<Literal>] key = 
    "dyfrig.machine"

(* Types *)

type MachineInspection =
    { Execution: ExecutionLog list }

and ExecutionLog =
    | Action of ActionLog
    | Decision of DecisionLog
    | Handler of HandlerLog
    | Operation of OperationLog

and ActionLog =
    { Name: string
      Overridden: bool }

and DecisionLog =
    { Name: string
      Overridden: bool
      Result: bool
      Next: string }

and HandlerLog =
    { Name: string
      Overridden: bool }

and OperationLog =
    { Name: string }

(* Constructors *)

let private machineInspection =
    { Execution = List.empty }

(* Lenses *)

let executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

(* Functions *)

let initI : OwinMonad<unit> =
    setI key machineInspection

let readI : OwinMonad<MachineInspection option> =
    getI key

let executionI e =
    modI key (modL executionLens (fun es -> e :: es))