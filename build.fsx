#I "packages/FAKE/tools"
#r "FakeLib.dll"

open System
open System.IO
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper

(* Types

   Types to help declaratively define the Freya solution, to enable strongly typed
   access to all properties and data required for the varying builds. *)

type Solution =
    { Name: string
      Metadata: Metadata
      Structure: Structure }

and Metadata =
    { Summary: string
      Description: string
      Authors: string list
      Keywords: string list
      Info: Info }

and Info =
    { ReadMe: string
      License: string
      Notes: string }

and Structure =
    { Solution: string
      Projects: Projects }

and Projects =
    { Source: SourceProject list
      Test: TestProject list }

and SourceProject =
    { Name: string
      Dependencies: Dependency list }

and Dependency =
    | Package of string
    | Local of string

and TestProject =
    { Name: string }

(* Data

   The Freya solution expressed as a strongly typed structure using the previously
   defined type system. *)

let freya =
    { Name = "freya"
      Metadata =
        { Summary = "Freya"
          Description = "Freya"
          Authors =
            [ "Ryan Riley (@panesofglass)"
              "Andrew Cherry (@kolektiv)" ]
          Keywords =
            [ "f#"
              "fsharp"
              "web"
              "owin"
              "http"
              "machine" ]
          Info =
            { ReadMe = "README.md"
              License = "LICENSE.txt"
              Notes = "RELEASE_NOTES.md" } }
      Structure =
        { Solution = "Freya.sln"
          Projects =
            { Source =
                [ { Name = "Freya.Core"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether" ] }
                  { Name = "Freya.Machine"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Fleece"
                          Local "Freya.Core"
                          Local "Freya.Pipeline"
                          Local "Freya.Recorder"
                          Local "Freya.Types.Cors"
                          Local "Freya.Types.Http"
                          Local "Freya.Types.Language"
                          Local "Freya.Types.Uri" ] }
                  { Name = "Freya.Pipeline"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Local "Freya.Core" ] }
                  { Name = "Freya.Recorder"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Fleece"
                          Local "Freya.Core" ] }
                  { Name = "Freya.Router"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Fleece"
                          Local "Freya.Core"
                          Local "Freya.Pipeline"
                          Local "Freya.Recorder"
                          Local "Freya.Types.Http" ] }
                  { Name = "Freya.Types"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "FParsec" ] }
                  { Name = "Freya.Types.Cors"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "FParsec"
                          Local "Freya.Types"
                          Local "Freya.Types.Http"
                          Local "Freya.Types.Uri" ] }
                  { Name = "Freya.Types.Http"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "FParsec"
                          Local "Freya.Core"
                          Local "Freya.Types"
                          Local "Freya.Types.Language"
                          Local "Freya.Types.Uri" ] }
                  { Name = "Freya.Types.Language"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "FParsec"
                          Local "Freya.Types" ] }
                  { Name = "Freya.Types.Uri"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "FParsec"
                          Local "Freya.Types" ] } ]
              Test =
                [ { Name = "Freya.Core.Tests" }
                  { Name = "Freya.Machine.Tests" }
                  { Name = "Freya.Pipeline.Tests" }
                  { Name = "Freya.Router.Tests" }
                  { Name = "Freya.Types.Tests" }
                  { Name = "Freya.Types.Cors.Tests" }
                  { Name = "Freya.Types.Http.Tests" }
                  { Name = "Freya.Types.Language.Tests" }
                  { Name = "Freya.Types.Uri.Tests" } ] } } }

(* Properties

   Computed properties of the build based on existing data structures and/or
   environment variables, creating a derived set of properties. *)

let appveyor =
    environVar "APPVEYOR" <> null

let release =
    parseReleaseNotes (File.ReadAllLines freya.Metadata.Info.Notes)

let assemblyVersion =
    release.AssemblyVersion

let nugetVersion =
    match appveyor, release.NugetVersion.Contains "-" with
    | true, true -> sprintf "%s%s" release.NugetVersion buildVersion
    | true, _ -> sprintf "%s.%s" release.NugetVersion buildVersion
    | _ -> release.NugetVersion

let notes =
    String.concat Environment.NewLine release.Notes

(* Targets

   FAKE targets expressing the components of a Freya build, to be assembled
   in to specific usable targets subsequently. *)

(* Publish *)

let dependencies (x: SourceProject) =
    x.Dependencies 
    |> List.map (function | Package x -> x, GetPackageVersion "packages" x
                          | Local x -> x, nugetVersion)

let extensions =
    [ "dll"
      "pdb"
      "xml" ]

let files (x: SourceProject) =
    extensions
    |> List.map (fun ext ->
        (sprintf @"..\src\%s\bin\Release\%s.%s" x.Name x.Name ext), 
         Some "lib/net40", 
         None)

let tags (s: Solution) =
    String.concat " " s.Metadata.Keywords

Target "Publish.Packages" (fun _ ->
    freya.Structure.Projects.Source |> List.iter (fun project ->
        NuGet (fun x ->
            { x with
                Authors = freya.Metadata.Authors
                Dependencies = dependencies project
                Description = freya.Metadata.Description
                Files = files project
                OutputPath = "bin"
                Project = project.Name
                ReleaseNotes = notes
                Summary = freya.Metadata.Summary
                Tags = tags freya
                Version = nugetVersion }) "nuget/template.nuspec"))

(* Source *)

Target "Source.AssemblyInfo" (fun _ ->
    freya.Structure.Projects.Source
    |> List.iter (fun p ->
        CreateFSharpAssemblyInfo (sprintf @"src/%s/AssemblyInfo.fs" p.Name)
            [ Attribute.Description freya.Metadata.Summary
              Attribute.FileVersion assemblyVersion
              Attribute.Product p.Name
              Attribute.Title p.Name
              Attribute.Version assemblyVersion ]))

Target "Source.Build" (fun _ ->
    build (fun x ->
        { x with
            Properties =
                [ "Optimize",      environVarOrDefault "Build.Optimize"      "True"
                  "DebugSymbols",  environVarOrDefault "Build.DebugSymbols"  "True"
                  "Configuration", environVarOrDefault "Build.Configuration" "Release" ]
            Targets =
                [ "Build" ]
            Verbosity = Some Quiet }) freya.Structure.Solution)

Target "Source.Clean" (fun _ ->
    CleanDirs [
        "bin"
        "temp" ])

Target "Source.Test" (fun _ ->
    freya.Structure.Projects.Test 
    |> List.map (fun x -> sprintf "tests/%s/bin/Release/%s.dll" x.Name x.Name)
    |> NUnitParallel (fun x ->
        { x with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" }))

(* Builds

   Specifically defined dependencies to produce usable builds for varying scenarios,
   such as CI, documentation, etc. *)

Target "Default" DoNothing
Target "Source" DoNothing
Target "Publish" DoNothing

(* Publish *)

"Source"
=?> ("Publish.Packages", not isMono)
==> "Publish"

(* Source *)

"Source.Clean"
==> "Source.AssemblyInfo"
==> "Source.Build"
==> "Source.Test"
==> "Source"

(* Default *)

"Source"
==> "Publish"
==> "Default"

(* Run *)

RunTargetOrDefault "Default"
