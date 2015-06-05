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
                  { Name = "Freya.Lenses.Http"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Arachne.Http"
                          Local "Freya.Core" ] }
                  { Name = "Freya.Lenses.Http.Cors"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Arachne.Http.Cors"
                          Local "Freya.Core"
                          Local "Freya.Lenses.Http" ] }
                  { Name = "Freya.Machine"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Chiron"
                          Package "Hekate"
                          Local "Freya.Core"
                          Local "Freya.Recorder" ] }
                  { Name = "Freya.Machine.Extensions.Http"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Arachne.Http"
                          Local "Freya.Core"
                          Local "Freya.Lenses.Http"
                          Local "Freya.Machine" ] }
                  { Name = "Freya.Machine.Extensions.Http.Cors"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Arachne.Http.Cors"
                          Local "Freya.Core"
                          Local "Freya.Lenses.Http"
                          Local "Freya.Lenses.Http.Cors"
                          Local "Freya.Machine"
                          Local "Freya.Machine.Extensions.Http" ] }
                  { Name = "Freya.Machine.Router"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Local "Freya.Core"
                          Local "Freya.Machine"
                          Local "Freya.Router" ] }
                  { Name = "Freya.Recorder"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Chiron"
                          Package "Arachne.Core"
                          Local "Freya.Core" ] }
                  { Name = "Freya.Router"
                    Dependencies =
                        [ Package "FSharp.Core"
                          Package "Aether"
                          Package "Chiron"
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
          Raw = "https://raw.github.com/freya-fs" } }

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
        let parts = release.NugetVersion.Split([|'-'|])
        if Array.length parts = 2 then
            sprintf "%s.%s-%s" parts.[0] buildVersion parts.[1]
        else sprintf "%s.%s" release.NugetVersion buildVersion
    else release.NugetVersion

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
         sprintf @"..\src\%s\bin\Release\%s.%s" x.Name x.Name ext,
         Some "lib/net45", 
         None)

let projectFile (x: SourceProject) =
    sprintf @"src/%s/%s.fsproj" x.Name x.Name

let tags (s: Solution) =
    String.concat " " s.Metadata.Keywords

#if MONO
#else
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"

open SourceLink

Target "Publish.Debug" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" freya.VersionControl.Raw (freya.Name.ToLowerInvariant ())

    freya.Structure.Projects.Source
    |> List.iter (fun project ->
        use git = new GitRepo __SOURCE_DIRECTORY__

        let release = VsProj.LoadRelease (projectFile project)
        let files = release.Compiles -- "**/AssemblyInfo.fs"

        git.VerifyChecksums files
        release.VerifyPdbChecksums files
        release.CreateSrcSrv baseUrl git.Commit (git.Paths files)
        
        Pdbstr.exec release.OutputFilePdb release.OutputFilePdbSrcSrv))

Target "Publish.MetaPackage" (fun _ ->
    NuGet (fun x ->
        { x with
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Authors = freya.Metadata.Authors
            Dependencies =
                freya.Structure.Projects.Source
                |> List.map (fun project ->
                    project.Name, nugetVersion)
            Description = freya.Metadata.Description
            Files = List.empty
            OutputPath = "bin"
            Project = "Freya"
            Publish = hasBuildParam "nugetkey"
            ReleaseNotes = notes
            Summary = freya.Metadata.Summary
            Tags = tags freya
            Version = nugetVersion }) "nuget/template.nuspec")

Target "Publish.Packages" (fun _ ->
    freya.Structure.Projects.Source 
    |> List.iter (fun project ->
        NuGet (fun x ->
            { x with
                AccessKey = getBuildParamOrDefault "nugetkey" ""
                Authors = freya.Metadata.Authors
                Dependencies = dependencies project
                Description = freya.Metadata.Description
                Files = files project
                OutputPath = "bin"
                Project = project.Name
                Publish = hasBuildParam "nugetkey"
                ReleaseNotes = notes
                Summary = freya.Metadata.Summary
                Tags = tags freya
                Version = nugetVersion }) "nuget/template.nuspec"))

#endif

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

"Source"
#if MONO
#else
==> "Publish.Debug"
==> "Publish.Packages"
==> "Publish.MetaPackage"
#endif
==> "Publish"

(* Source *)

"Source.Clean"
==> "Source.GitSubmodules"
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
