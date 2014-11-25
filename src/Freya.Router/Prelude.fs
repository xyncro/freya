[<AutoOpen>]
module internal Freya.Router.Prelude

open Aether

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box

let inline flip f a b = 
    f b a