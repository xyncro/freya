(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "Newtonsoft.Json.dll"
#r "System.Net.Http.dll"
#r "System.Net.Http.Formatting.dll"
#r "System.Net.Http.WebRequest.dll"
#r "System.Web.Http.dll"
#r "FSharpx.Core.dll"
#r "Frank.dll"
open System.Net
open System.Net.Http
open Frank

(**
Frank
===================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Frank library can be <a href="https://nuget.org/packages/Frank">installed from NuGet</a>:
      <pre>PM> Install-Package Frank</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this library.

*)

let helloWorld request =
    OK ignore <| Str "Hello, world!"
    |> async.Return

(**
Samples & documentation
-----------------------

The library <s>comes</s> will soon come with comprehensive documentation and
includes a [Getting Started tutorial](tutorial.html), as well as an [API reference](reference/index.html).
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into documentation. You might
also want to read [library design notes][readme] to understand how it works.

The library is available under the Apache 2.0 license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/frank-fs/frank/tree/master/docs/content
  [gh]: https://github.com/frank-fs/frank
  [issues]: https://github.com/frank-fs/frank/issues
  [readme]: https://github.com/frank-fs/frank/blob/master/README.md
  [license]: https://github.com/frank-fs/frank/blob/master/LICENSE.txt
*)
