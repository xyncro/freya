#I "packages/FAKE/tools"
#r "FakeLib.dll"

open System
open System.IO
open Fake
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let (!!) includes = (!! includes).SetBaseDirectory __SOURCE_DIRECTORY__

(* Properties

   Computed properties of the build based on existing data structures and/or
   environment variables, creating a derived set of properties. *)

let project = "Freya"
let summary = "Freya - A Functional-First F# Web Stack"
let description = "Freya - A Functional-First F# Web Stack"
let authors = "Ryan Riley (@panesofglass), Andrew Cherry (@kolektiv)"
let solutionFile = "Freya.sln"
let testAssemblies = "tests/**/bin/Release/*Tests*dll"
let gitHome = "https://github.com/freya-fs"
let gitName = "freya"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/freya-fs"

let release =
    parseReleaseNotes (File.ReadAllLines "RELEASE_NOTES.md")

let assemblyVersion =
    release.AssemblyVersion

let isAppVeyorBuild =
    environVar "APPVEYOR" <> null

let majorMinorVersion (version: string) =
    let parts = version.Split([|'.'|])
    sprintf "%s.%s" parts.[0] parts.[1]

let nugetVersion =
    if isAppVeyorBuild then
        let parts = release.NugetVersion.Split([|'-'|])
        if Array.length parts = 2 then
            sprintf "%s.%s-%s" (majorMinorVersion parts.[0]) buildVersion parts.[1]
        else sprintf "%s.%s" (majorMinorVersion release.NugetVersion) buildVersion
    else release.NugetVersion

let notes =
    String.concat Environment.NewLine release.Notes

(* Targets

   FAKE targets expressing the components of a Freya build, to be assembled
   in to specific usable targets subsequently. *)

(* Publish *)

#if MONO
#else
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"

open SourceLink

Target "Publish.Debug" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw gitName
    use repo = new GitRepo(__SOURCE_DIRECTORY__)
    !! "src/**/*.fsproj"
    |> Seq.iter (fun f ->
        let proj = VsProj.LoadRelease f
        logfn "source linking %s" proj.OutputFilePdb
        let files = proj.Compiles -- "**/AssemblyInfo.fs"
        repo.VerifyChecksums files
        proj.VerifyPdbChecksums files
        proj.CreateSrcSrv baseUrl repo.Commit (repo.Paths files)
        Pdbstr.exec proj.OutputFilePdb proj.OutputFilePdbSrcSrv))

Target "Publish.Packages" (fun _ ->
    Paket.Pack (fun x ->
        { x with
            OutputPath = "./bin"
            ReleaseNotes = notes
            Version = nugetVersion }) )

#endif

(* Source *)

Target "Source.GitSubmodules" (fun _ ->
    Git.CommandHelper.gitCommand "." "submodule update --init")

let assemblyInfo (name: string) =
    sprintf "src/%s/AssemblyInfo.fs" name

Target "Source.AssemblyInfo" (fun _ ->
    !! "src/**/*.fsproj"
    |> Seq.iter (fun project ->
        CreateFSharpAssemblyInfo (assemblyInfo project)
            [ Attribute.Company authors
              Attribute.Description summary
              Attribute.FileVersion assemblyVersion
              Attribute.InformationalVersion nugetVersion
              Attribute.Product project
              Attribute.Title project
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
            Verbosity = Some Quiet }) solutionFile)

Target "Source.Clean" (fun _ ->
    CleanDirs [
        "bin"
        "temp" ])

Target "Source.Test" (fun _ ->
    try
        !! testAssemblies
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
