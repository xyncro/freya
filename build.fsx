#if BOOT
open Fake
module FB = Fake.Boot
FB.Prepare {
    FB.Config.Default __SOURCE_DIRECTORY__ with
        NuGetDependencies =
            let (!!) x = FB.NuGetDependency.Create x
            [
                !!"FAKE"
                !!"NuGet.Build"
                !!"NuGet.Core"
            ]
}
#endif

#load ".build/boot.fsx"

open System.IO
open Fake 
open Fake.AssemblyInfoFile
open Fake.MSBuild

// properties
let projectName = "Dyfrig"
let version = if isLocalBuild then "0.1." + System.DateTime.UtcNow.ToString("yMMdd") else buildVersion
let projectSummary = "F# support for the Open Web Interface for .NET"
let projectDescription = projectSummary
let authors = ["Ryan Riley"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "http://github.com/panesofglass/Dyfrig"
let license = "http://github.com/panesofglass/Dyfrig/raw/master/LICENSE.txt"

// directories
let buildDir = __SOURCE_DIRECTORY__ @@ "build"
let testDir = __SOURCE_DIRECTORY__ @@ "test"
let deployDir = __SOURCE_DIRECTORY__ @@ "deploy"
let packagesDir = __SOURCE_DIRECTORY__ @@ "packages"
let nugetDir = __SOURCE_DIRECTORY__ @@ "nuget"
let nugetLib = nugetDir @@ "lib/net40"
let template = __SOURCE_DIRECTORY__ @@ "template.html"
let sources = __SOURCE_DIRECTORY__ @@ "src"
let docsDir = __SOURCE_DIRECTORY__ @@ "docs"
let docRoot = getBuildParamOrDefault "docroot" homepage

// tools
let nugetPath = "./.nuget/nuget.exe"

// files
let appReferences =
    !! "src/**/*.fsproj"
    
let testReferences =
    !! "tests/**/*.fsproj"

// targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir
               testDir
               docsDir
               deployDir
               nugetDir
               nugetLib]
)

Target "BuildApp" (fun _ ->
    if not isLocalBuild then
        [ Attribute.Version(buildVersion)
          Attribute.Title(projectName)
          Attribute.Description(projectDescription)
          Attribute.Guid("36d091d6-791f-4d45-a81a-8d51e4822f57")
        ]
        |> CreateFSharpAssemblyInfo "src/AssemblyInfo.fs"

    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTests" (fun _ ->
    MSBuildDebug testDir "Build" testReferences
        |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
    let errorCode =
        let program = testDir @@ "Dyfrig.Tests.exe"
        let p, a = if not isMono then program, null else "mono", program
        asyncShellExec { defaultParams with Program = p; CommandLine = a }
        |> Async.RunSynchronously
    if errorCode <> 0 then failwith "Error in tests"
)

Target "CopyLicense" (fun _ ->
    [ "LICENSE.txt" ] |> CopyTo buildDir
)

Target "CreateNuGet" (fun _ ->
    [ buildDir @@ "Dyfrig.dll"
      buildDir @@ "Dyfrig.pdb" ]
        |> CopyTo nugetLib

    NuGet (fun p -> 
            {p with               
                Authors = authors
                Project = projectName
                Description = projectDescription
                Version = version
                OutputPath = nugetDir
                ToolPath = nugetPath
                AccessKey = getBuildParamOrDefault "nugetkey" ""
                Publish = hasBuildParam "nugetKey" })
        "Dyfrig.nuspec"

    !! (nugetDir @@ sprintf "Dyfrig.%s.nupkg" version)
        |> CopyTo deployDir
)

Target "Deploy" DoNothing
Target "Default" DoNothing

// Build order
"Clean"
  ==> "BuildApp" <=> "CopyLicense" <=> "BuildTests"
  ==> "Test"
  ==> "CreateNuGet"
  ==> "Deploy"

"Default" <== ["Deploy"]

// Start build
RunTargetOrDefault "Default"

