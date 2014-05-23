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
open System
open System.Net
open System.Net.Http
open System.Web.Http
open System.Web.Http.HttpResource
open FSharp.Control
open Frank

(**
Getting Started
========================

In this tutorial, we'll explore the basics of building and hosting Web APIs with Frank.

### Define an Application

One may define a web application interface using a large variety of signatures.
Indeed, if you search the web, you're likely to find a large number of approaches.
When starting with Frank, I wanted to try to find a way to define an HTTP application
using pure functions and function composition. The closest I found was the following:
*)

type HttpApplication = HttpRequestMessage -> Async<HttpResponseMessage>
    
let orElse left right = fun request -> Option.orElse (left request) (right request)
let inline (<|>) left right = orElse left right

(**
The last of these was a means for merging multiple applications together into a single
application. This allowed for a nice symmetry and elegance in that everything you composed
would always have the same signature. Additional functions would allow you to map
applications to specific methods or uri patterns.

A "Hello, world!" application using these signatures would look like the following:
*)

let helloWorld request =
    OK ignore <| Str "Hello, world!"
    |> async.Return

(**
A simple echo handler that returns the same input as it received might look like the following:
*)

let echo (request: HttpRequestMessage) = async {
    let! content = Async.AwaitTask <| request.Content.ReadAsStringAsync()
    return respond HttpStatusCode.OK
           <| ``Content-Type`` "text/plain"
           <| new StringContent(content)
}

(**
or just:
*)

let echo (request: HttpRequestMessage) =
    OK <| ``Content-Type`` "text/plain"
       <| (Async.AwaitTask <| request.Content.ReadAsStringAsync())

(**
If you want to provide content negotiation, use:
*)

let echo = runConneg formatters <| fun request ->
    Async.AwaitTask <| request.Content.ReadAsStringAsync()

(**
### Define an HTTP Resource

Alas, this approach works only so well. HTTP is a rich communication specification.
The simplicity and elegance of a purely functional approach quickly loses the ability
to communicate back options to the client. For instance, given the above, how do you
return a meaningful `405 Method Not Allowed` response? The HTTP specification requires
that you list the allowed methods, but if you merge all the logic for selecting an
application into the functions, there is no easy way to recall all the allowed methods,
short of trying them all. You could require that the developer add the list of used
methods, but that, too, misses the point that the application should be collecting this
and helping the developer by taking care of all of the nuts and bolts items.

The next approach I tried involved using a tuple of a list of allowed HTTP methods and
the application handler, which used the merged function approach described above for
actually executing the application. However, once again, there are limitations. This
structure accurately represents a resource, but it does not allow for multiple resources
to coexist side-by-side. Another tuple of uri pattern matching expressions could wrap
a list of these method * handler tuples, but at this point I realized I would be better
served by using real types and added an `HttpResource` type to ease the type burden.

HTTP resources expose an resource handler function at a given uri.
In the common MVC-style frameworks, this would roughly correspond
to a `Controller`. Resources should represent a single entity type,
and it is important to note that a `Foo` is not the same entity
type as a `Foo list`, which is where the typical MVC approach goes wrong. 

The ``405 Method Not Allowed`` function allows a resource to correctly respond to messages.
Therefore, we extend the `HttpResource` with an `Invoke` method.
Also note that the methods will always be looked up using the latest set. This could
probably be memoized so as to save a bit of time, but it allows us to ensure that all
available methods are reported.

### Compose Applications and Resources into Applications

A compositional approach to type mapping and handler design.
Here, the actual function shows clearly that we are really using
the `id` function to return the very same result.
*)

let echo2Transform = id

(**
The `echo2ReadRequest` maps the incoming request to a value that can be used
within the actual computation, or `echo2Transform` in this example.
*)

let echo2ReadRequest (request: HttpRequestMessage) =
    Async.AwaitTask <| request.Content.ReadAsStringAsync()

(**
The `echo2Respond` maps the outgoing message body to an HTTP response.
*)

let echo2Respond body =
    respond <| ``Content-Type`` "text/plain" <| body

(**
This `echo2` is the same in principle as `echo` above, except that the
logic for the message transform deals only with the concrete types
about which it cares and isn't bothered by the transformations.
*)

let echo2 request = async {
    let! content = echo2ReadRequest request
    let body = echo2Transform content
    return echo2Respond <| new StringContent(body)
}

(**
Create a `HttpResource` instance at the root of the site that responds to `POST`.
*)

let resource = route "/" <| post echo2

(**
Other combinators are available to handle other scenarios, such as:

1. Content Negotiation
2. Building Responses
3. Combining applications into resources
4. Combining resources into applications

Check the samples for more examples of these combinators.

### Define a Middleware

Middlewares follow a Russian-doll model for wrapping resource handlers with additional functionality. Frank middlewares take an `HttpApplication` and return an `HttpApplication`.
The `Frank.Middleware` module defines several, simple middlewares, such as the `log` middleware that intercepts logs incoming requests and the time taken to respond:
*)

let log app = fun (request : HttpRequestMessage) -> async {
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let! response = app request
    printfn "Received a %A request from %A. Responded in %i ms."
            request.Method.Method
            request.RequestUri.PathAndQuery
            sw.ElapsedMilliseconds
    sw.Reset()
    return response
}

(**
The most likely place to insert middlewares is the outer edge of your application. However, since middlewares are themselves just `HttpApplication`s, you can compose them into a Frank application at any level. Want to support logging only on one troublesome resource? No problem. Expose the resource as an application, wrap it in the log middleware, and insert it into the larger application as you did before.

## Hosting

Frank will run on any hosting platform that supports the
[Web API](http://asp.net/web-api/) library. To hook up your Frank application,
use the `register` function, passing in the resources and the instance of `HttpConfiguration`.
*)

register [resource] config

(**
This extension adds a default route to your `HttpConfiguration` instance and
adds a `DelegatingHandler` instance to the route's `HttpConfiguration.MessageHandlers` collection.
*)
