[<AutoOpen>]
module internal Freya.Machine.Prelude

(* Equality/Comparison

   Functions for simplifying the customization of equality
   and comparison on types where this is required. *)

let equalsOn f x (y: obj) =
    match y with
    | :? 'T as y -> (f x = f y)
    | _ -> false
 
let hashOn f x =  hash (f x)
 
let compareOn f x (y: obj) =
    match y with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "y" "cannot compare values of different types"