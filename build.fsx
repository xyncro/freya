// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------
#I "packages/FAKE/tools"
#r "NuGet.Core.dll"
#r "FakeLib.dll"

open System
open System.IO
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
#if MONO
#else
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"
open SourceLink
#endif


// --------------------------------------------------------------------------------------
// Provide project-specific details below
// --------------------------------------------------------------------------------------
// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package 
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"
// The name of the project 
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "Freya"
// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "F# support for the Open Web Interface for .NET"
// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = """
  F# support for the Open Web Interface for .NET"""
// List of author names (for NuGet package)
let authors = [ "Ryan Riley"; "Andrew Cherry" ]
// Tags for your project (for NuGet package)
let tags = "F# fsharp web http owin"
// File system information 
// (<solutionFile>.sln is built during the building process)
let solutionFile = "Freya.sln"
// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"
// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted 
let gitHome = "https://github.com/freya-fs"
// The name of the project on GitHub
let gitName = "freya"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/freya-fs"

// --------------------------------------------------------------------------------------
// The rest of the file includes standard build steps 
// --------------------------------------------------------------------------------------
// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let (!!) includes = (!!includes).SetBaseDirectory __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")
let isAppVeyorBuild = environVar "APPVEYOR" <> null

let nugetVersion = 
    if isAppVeyorBuild then 
        // If `release.NugetVersion` includes a pre-release suffix, just append the `buildVersion`.
        if release.NugetVersion.Contains("-") then sprintf "%s%s" release.NugetVersion buildVersion
        else sprintf "%s.%s" release.NugetVersion buildVersion
    else release.NugetVersion

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ -> 
    let fileName = "src/" + project + "/AssemblyInfo.fs"
    CreateFSharpAssemblyInfo fileName [ Attribute.Title project
                                        Attribute.Product project
                                        Attribute.Description summary
                                        Attribute.Version release.AssemblyVersion
                                        Attribute.FileVersion release.AssemblyVersion ])

Target "BuildVersion" (fun _ -> Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" nugetVersion) |> ignore)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ -> CleanDirs [ "bin"; "temp" ])

Target "CleanDocs" (fun _ -> CleanDirs [ "docs/output" ])

// --------------------------------------------------------------------------------------
// Build library & test project

let setParams defaults =
    { defaults with
        Verbosity = Some(Quiet)
        Targets = ["Rebuild"]
        Properties =
            [ "Optimize", "True"
              "DebugSymbols", "True"
              "Configuration", "Release" ]}

Target "Build" (fun _ -> build setParams solutionFile)

Target "CopyFiles" (fun _ -> 
    [ "LICENSE.txt" ] |> CopyTo "bin"
    !!("src/" + project + "/bin/Release/Freya*.*") |> CopyTo "bin")

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ -> 
    !!testAssemblies
    |> NUnit(fun p -> 
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" }))

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries https://github.com/ctaggart/SourceLink
Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw (project.ToLower())
    use repo = new GitRepo(__SOURCE_DIRECTORY__)
    !! "src/**/*.fsproj"
    |> Seq.iter (fun f ->
        let proj = VsProj.LoadRelease f
        logfn "source linking %s" proj.OutputFilePdb
        let files = proj.Compiles -- "**/AssemblyInfo.fs"
        repo.VerifyChecksums files
        proj.VerifyPdbChecksums files
        proj.CreateSrcSrv baseUrl repo.Revision (repo.Paths files)
        Pdbstr.exec proj.OutputFilePdb proj.OutputFilePdbSrcSrv
    )
)
#endif

// --------------------------------------------------------------------------------------
// Build NuGet packages

Target "PackageCore" (fun _ -> 
    NuGet (fun p -> 
        { p with
            Authors = authors
            Project = "Freya.Core"
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies =
                [ "FSharp.Core", GetPackageVersion "packages" "FSharp.Core"
                  "Aether",      GetPackageVersion "packages" "Aether" ]
            Files = 
                [ (@"..\src\Freya.Core\bin\Release\Freya.Core.dll", Some "lib/net40", None)
                  (@"..\src\Freya.Core\bin\Release\Freya.Core.xml", Some "lib/net40", None)
                  (@"..\src\Freya.Core\bin\Release\Freya.Core.pdb", Some "lib/net40", None) ]
        }) ("nuget/Freya.Core.nuspec"))

Target "PackageTypes" (fun _ ->
    NuGet (fun p -> 
        { p with
            Authors = authors
            Project = "Freya.Types"
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.Join(Environment.NewLine, release.Notes)
            Tags = tags
            OutputPath = "bin"
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Dependencies =
                [ "FSharp.Core", GetPackageVersion "packages" "FSharp.Core"
                  "Aether",      GetPackageVersion "packages" "Aether"
                  "FParsec",     GetPackageVersion "packages" "FParsec"
                  "Freya.Core",  release.NugetVersion ]
            Files = 
                [ (@"..\src\Freya.Types\bin\Release\Freya.Types.dll", Some "lib/net40", None)
                  (@"..\src\Freya.Types\bin\Release\Freya.Types.xml", Some "lib/net40", None)
                  (@"..\src\Freya.Types\bin\Release\Freya.Types.pdb", Some "lib/net40", None) ]
        }) ("nuget/Freya.Core.nuspec"))

// TODO: Add additional NuGet packages for each library.

Target "BuildPackages" DoNothing

// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateReferenceDocs" 
    (fun _ -> 
    if not <| executeFSIWithArgs "docs/tools" "generate.fsx" [ "--define:RELEASE"; "--define:REFERENCE" ] [] then 
        failwith "generating reference documentation failed")

Target "GenerateHelp" 
    (fun _ -> 
    if not <| executeFSIWithArgs "docs/tools" "generate.fsx" [ "--define:RELEASE"; "--define:HELP" ] [] then 
        failwith "generating help documentation failed")

Target "GenerateDocs" DoNothing

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ -> 
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir)

Target "Release" (fun _ -> 
    StageAll ""
    Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""
    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
=?> ("BuildVersion", isAppVeyorBuild)
==> "AssemblyInfo"
==> "Build"
==> "RunTests"
==> "CopyFiles"
==> "All" 

"All"
=?> ("GenerateReferenceDocs", isLocalBuild && not isMono)
=?> ("GenerateDocs", isLocalBuild && not isMono) 
=?> ("ReleaseDocs", isLocalBuild && not isMono)

"All"
#if MONO
#else
//=?> ("SourceLink", Pdbstr.tryFind().IsSome )
#endif
==> "PackageCore"
==> "PackageTypes"
==> "BuildPackages"

"CleanDocs"
==> "GenerateHelp"
==> "GenerateReferenceDocs"
==> "GenerateDocs"

"ReleaseDocs" ==> "Release"
"BuildPackages" ==> "Release"

RunTargetOrDefault "All"
