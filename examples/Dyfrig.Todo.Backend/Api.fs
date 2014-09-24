module Dyfrig.Todo.Backend.Api

open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Machine
open Dyfrig.Pipeline
open Dyfrig.Pipeline.Operators
open Dyfrig.Router

// Resources

let todos =
    machine {
        allowedMethods [ OPTIONS ] } |> reifyMachine

let todo =
    machine {
        return () } |> reifyMachine

// Routes

let api =
    routes {
        route Any "/" todos
        route Any "/:id" todo } |> reifyRoutes

// CORS

let cors =
       setPLM (Response.header "Access-Control-Allow-Origin") [| "*" |]
    *> setPLM (Response.header "Access-Control-Allow-Headers") [| "Content-Type" |]
    *> next

// Pipeline

let app =
        cors 
    >?= api

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromOwinMonad (app)