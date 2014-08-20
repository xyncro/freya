namespace Dyfrig.Machine

open Dyfrig

[<AutoOpen>]
module internal Defaults =

    // Actions

    let defaultAction = 
        owin { 
            return () }

    // Configuration

    let defaultAllowedMethods =
        Set.ofList
            [ GET
              HEAD ]

    let defaultKnownMethods =
        Set.ofList 
            [ DELETE
              HEAD
              GET
              OPTIONS
              PATCH
              POST
              PUT
              TRACE ]

    // Decisions

    let defaultDecision (p: bool) = 
        owin { 
            return p }

    // Handlers

    let defaultHandler code phrase =
        owin {
            do! setPLM Response.StatusCode code
            do! setPLM Response.ReasonPhrase phrase

            return Array.empty<byte> }
