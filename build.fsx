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
      Structure: Structure
      VersionControl: VersionControl }

and Metadata =
    { Summary: string
      Description: string
      Owners: string list
      Authors: string list
      Keywords: string list
      Info: Info }

and Info =
    { ReadMe: string
      License: string
      Notes: string
      IconUrl: string }

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

and VersionControl =
    { Source: string
      Raw: string }

(* Data

   The Freya solution expressed as a strongly typed structure using the previously
   defined type system. *)

let freya =
    { Name = "Freya"
      Metadata =
        { Summary = "Freya - A Functional-First F# Web Stack"
          Description = "Freya - A Functional-First F# Web Stack"
          Owners =
            [ "kolektiv"
              "panesofglass" ]
          Authors =
            [ "Andrew Cherry (@kolektiv)"
              "Ryan Riley (@panesofglass)" ]
          Keywords =
            [ "f#"
              "fsharp"
              "web"
              "owin"
              "http"
              "machine" ]
          Info =
            { ReadMe = "README.md"
              License = "LICENSE.md"
              Notes = "RELEASE_NOTES.md"
              IconUrl = "docs/files/img/logo.png" } }
      Structure =
        { Solution = "Freya.sln"
          Projects =
            { Source =
                [ { Name = "Freya.Core"
                    Dependencies =
                        [ Package "Aether" ] }
                  { Name = "Freya.Lenses.Http"
                    Dependencies =
                        [ Package "Aether"
                          Package "Arachne.Http"
                          Local "Freya.Core" ] }
                  { Name = "Freya.Lenses.Http.Cors"
                    Dependencies =
                        [ Package "Aether"
                          Package "Arachne.Http.Cors"
                          Local "Freya.Core"
                          Local "Freya.Lenses.Http" ] }
                  { Name = "Freya.Machine"
                    Dependencies =
                        [ Package "Aether"
                          Package "Hekate"
                          Local "Freya.Core"
                          Local "Freya.Recorder" ] }
                  { Name = "Freya.Machine.Extensions.Http"
                    Dependencies =
                        [ Package "Aether"
                          Package "Arachne.Http"
                          Local "Freya.Core"
                          Local "Freya.Lenses.Http"
                          Local "Freya.Machine" ] }
                  { Name = "Freya.Machine.Extensions.Http.Cors"
                    Dependencies =
                        [ Package "Aether"
                          Package "Arachne.Http.Cors"
                          Local "Freya.Core"
                          Local "Freya.Lenses.Http"
                          Local "Freya.Lenses.Http.Cors"
                          Local "Freya.Machine"
                          Local "Freya.Machine.Extensions.Http" ] }
                  { Name = "Freya.Machine.Router"
                    Dependencies =
                        [ Local "Freya.Core"
                          Local "Freya.Machine"
                          Local "Freya.Router" ] }
                  { Name = "Freya.Recorder"
                    Dependencies =
                        [ Package "Aether"
                          Package "Arachne.Core"
                          Local "Freya.Core" ] }
                  { Name = "Freya.Router"
                    Dependencies =
                        [ Package "Aether"
                          Package "Hekate"
                          Package "Arachne.Http"
                          Package "Arachne.Uri.Template"
                          Local "Freya.Core"
                          Local "Freya.Recorder"
                          Local "Freya.Lenses.Http" ] } ]
              Test =
                [ { Name = "Freya.Core.Tests" }
                  { Name = "Freya.Machine.Tests" }
                  { Name = "Freya.Machine.Extensions.Http.Tests" }
                  { Name = "Freya.Router.Tests" } ] } }
      VersionControl =
        { Source = "https://github.com/freya-fs/freya"
          Raw = "https://raw.githubusercontent.com/freya-fs" } }

(* Properties

   Computed properties of the build based on existing data structures and/or
   environment variables, creating a derived set of properties. *)

let release =
    parseReleaseNotes (File.ReadAllLines freya.Metadata.Info.Notes)

let assemblyVersion =
    release.AssemblyVersion

let isAppVeyorBuild =
    environVar "APPVEYOR" <> null

let nugetVersion = 
    if isAppVeyorBuild then
        let nugetVersion =
            let isTagged = Boolean.Parse(environVar "APPVEYOR_REPO_TAG")
            if isTagged then
                environVar "APPVEYOR_REPO_TAG_NAME"
            else
                sprintf "%s-b%03i" release.NugetVersion (int buildVersion)
        Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" nugetVersion) |> ignore
        nugetVersion
    else release.NugetVersion

let notes =
    String.concat Environment.NewLine release.Notes

let githubRawUrl branch path =
    sprintf "%s/%s/%s/%s" freya.VersionControl.Raw "freya" branch path

let paketTemplateFile (x: SourceProject) =
    sprintf "src/%s/%s.fsproj.paket.template" x.Name x.Name

let generatePaketTemplate (project : SourceProject) =
    let lines =
        [|  yield "type project"
            yield "owners"
            for owner in freya.Metadata.Owners do
                yield "    " + owner
            yield "language en-US"
            yield "iconUrl " + (githubRawUrl "master" freya.Metadata.Info.IconUrl)
            yield "licenseUrl " + (githubRawUrl "master" freya.Metadata.Info.License)
            yield "projectUrl " + freya.VersionControl.Source
            yield "tags"
            for tag in freya.Metadata.Keywords do
                yield "    " + tag |]
    let text =
        lines
        |> Array.fold (fun (sb : Text.StringBuilder) line -> sb.AppendLine line) (Text.StringBuilder())
    File.WriteAllText(paketTemplateFile project, text.ToString())

let generateMetaPaketTemplate (projects : SourceProject list) =
    let lines =
        [|  yield "type file"
            yield "id " + freya.Name
            yield "title " + freya.Name
            yield "owners"
            for owner in freya.Metadata.Owners do
                yield "    " + owner
            yield "authors"
            for author in freya.Metadata.Authors do
                yield "    " + author
            yield "summary " + freya.Metadata.Summary
            yield "description"
            yield "    " + freya.Metadata.Description
            yield "dependencies"
            for project in projects do
                yield (sprintf "    %s >= %s" project.Name nugetVersion)
            yield "language en-US"
            yield "iconUrl " + (githubRawUrl "master" freya.Metadata.Info.IconUrl)
            yield "licenseUrl " + (githubRawUrl "master" freya.Metadata.Info.License)
            yield "projectUrl " + freya.VersionControl.Source
            yield "tags"
            for tag in freya.Metadata.Keywords do
                yield "    " + tag |]
    let text =
        lines
        |> Array.fold (fun (sb : Text.StringBuilder) line -> sb.AppendLine line) (Text.StringBuilder())
    File.WriteAllText("paket.template", text.ToString())

let generatePaketTemplates (projects : SourceProject list) =
    for project in projects do
        generatePaketTemplate project
    generateMetaPaketTemplate projects

(* Targets

   FAKE targets expressing the components of a Freya build, to be assembled
   in to specific usable targets subsequently. *)

(* Publish *)

let dependencies (x: SourceProject) =
    x.Dependencies 
    |> List.map (function | Package x -> x, GetPackageVersion "packages" x
                          | Local x -> x, nugetVersion)

let projectFile (x: SourceProject) =
    sprintf "src/%s/%s.fsproj" x.Name x.Name

#if MONO
#else
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"

open SourceLink

Target "Publish.Debug" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" freya.VersionControl.Raw (freya.Name.ToLowerInvariant ())

    freya.Structure.Projects.Source
    |> List.iter (fun project ->
        let release = VsProj.LoadRelease (projectFile project)
        let files = release.Compiles -- "**/AssemblyInfo.fs"
        SourceLink.Index files release.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl))
#endif

Target "Publish.Pack" (fun _ ->
    generatePaketTemplates freya.Structure.Projects.Source
    Paket.Pack (fun x ->
        { x with
            OutputPath = "bin"
            Version = nugetVersion
            ReleaseNotes = notes }))

Target "Publish.Push" (fun _ ->
    Paket.Push (fun p ->
        { p with WorkingDir = "bin" }))

(* Source *)

Target "Source.GitSubmodules" (fun _ ->
    Git.CommandHelper.gitCommand "." "submodule update --init")

let assemblyInfo (x: SourceProject) =
    sprintf @"src/%s/AssemblyInfo.fs" x.Name

let testAssembly (x: TestProject) =
    sprintf "tests/%s/bin/Release/%s.dll" x.Name x.Name

Target "Source.AssemblyInfo" (fun _ ->
    freya.Structure.Projects.Source
    |> List.iter (fun project ->
        CreateFSharpAssemblyInfo (assemblyInfo project)
            [ Attribute.Company (String.concat ", " freya.Metadata.Authors)
              Attribute.Description freya.Metadata.Summary
              Attribute.FileVersion assemblyVersion
              Attribute.InformationalVersion nugetVersion
              Attribute.Product project.Name
              Attribute.Title project.Name
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
    try
        freya.Structure.Projects.Test
        |> List.map (fun project -> testAssembly project)
        |> NUnit (fun x ->
            { x with
                DisableShadowCopy = true
                TimeOut = TimeSpan.FromMinutes 20.
                OutputFile = "bin/TestResults.xml" })
    finally
        AppVeyor.UploadTestResultsXml AppVeyor.TestResultsType.NUnit "bin")

(* Builds

   Specifically defined dependencies to produce usable builds for varying scenarios,
   such as CI, documentation, etc. *)

Target "Default" DoNothing
Target "Source" DoNothing
Target "Publish" DoNothing

(* Publish *)

"Default"
==> "Publish.Push"
==> "Publish"

(* Default *)

"Source"
=?> ("Publish.Debug", not isMono)
==> "Publish.Pack"
==> "Default"

(* Source *)

"Source.Clean"
==> "Source.GitSubmodules"
==> "Source.AssemblyInfo"
==> "Source.Build"
==> "Source.Test"
==> "Source"

(* Run *)

RunTargetOrDefault "Default"
