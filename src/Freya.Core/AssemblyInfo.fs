namespace System
open System.Reflection

[<assembly: AssemblyDescriptionAttribute("Freya")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
[<assembly: AssemblyProductAttribute("Freya.Core")>]
[<assembly: AssemblyTitleAttribute("Freya.Core")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0"
