### New in 2.0.202 - (Released 2015-12-11)
* Reset dependencies to correct versions. 2.0.201 accidentally included changes that were not intended for the 2.0.* release.

### New in 2.0.201 - (Released 2015-12-01)
* Fixed Last Modified, If-Modified-Since and If-Unmodified-Since headers

### New in 2.0.200 - (Released 2015-10-12)
* Updated dependencies, and removal of FSharp.Core from produced package dependencies

### New in 2.0.0 - (Released 2015-07-24)
* Lens naming changed to conform to emerging consensus of <Property>_ styling
* Internal refactoring for greater clarity
* Updated version of Arachne with additional lenses in to URI types

### New in 1.0.0 - (Released 2015-06-05)
* `Arachne` moved to `Arachne`
* `Freya.Types.Http` renamed `Freya.Lenses.Http` and now takes a dependency on `Arachne.Http` and exposes only the Freya lenses
* `Freya.Types.Http.Cors` renamed `Freya.Lenses.Http.Cors` and now takes a dependency on `Arachne.Http.Cors` and exposes only the Freya lenses

### New in 0.11.0-alpha - (Released 2015-05-04)
* Breaking changes:
  * `Freya.Pipeline` merged into `Freya.Core`
  * `Freya.Integration` merged into `Freya.Core`
  * `Freya.Pipeline` and `Freya.Integration` are no longer separate packages
  * `Freya.Pipeline` is now `Freya.Core.Pipeline` and automatically opened when `Freya.Core` is opened
  * `Freya.Integration` is its own module named `Freya.Core.Integration`, automatically opened when `Freya.Core` is opened, and no longer split into `Freya.Core.Integration` and `Freya.Pipeline.Integration`
  * `Arachne.Uri.Template` introduced
  * `Freya.Router` now uses URI Templates as route specifications, along with a new internal graph model

### New in 0.10.0-alpha - (Released 2015-03-21)
* Updated Arachne.* family
* Naming convention updates and simplification
* Experimental release of URI Template implementation (not for primetime!)

### New in 0.9.0-alpha - (Released 2015-03-09)
* OWIN integration for applications and middleware #28
* Experimental release of new Freya Stack

### New in 0.8.0 - (Released 2014/10/12)
* Removed `OwinRailway`. A new pipeline approach will appear as a separate library soon.
* Moved `SystemNetHttpAdapter` to its own library, `Freya.Net.Http`, which is not yet available.
* Expect `Freya` to be renamed `Freya.Core` in the near future.
* Now using [`Paket`](https://fsprojects.github.io/Paket/).

### New in 0.7.0 - (Released 2014/08/27)
* Reorganized modules, merging `Owin` with `Monad` into `OwinMonad`
* Added signature file for `OwinApp`
* Breaking changes to signatures found in `OwinMonad`:
    * Renamed `Owin.async` to `OwinMonad.fromAsync`
    * Added `OwinMonad.result`
    * Renamed `Owin.composeSeq` to `OwinMonad.bind`
    * `OwinMonad` is not automatically opened as was `Owin` previously

### New in 0.6.0 - (Released 2014/08/27)
* Added `Environment.flush` function to support pushing changes to an immutable `Environment` to the original `OwinEnv`.
* Added additional properties to support upcoming [OWIN v1.1](https://github.com/owin/owin/blob/master/spec/owin-1.1.0.md) keys:
    * [`owin.RequestId`](https://github.com/owin/owin/issues/18)
    * [`owin.RequestUser`](https://github.com/owin/owin/issues/9)

### New in 0.5.0 - (Released 2014/08/011)
* Added OwinMonad (courtesy of [Andrew Cherry](https://github.com/kolektiv))
* Breaking change: `OwinApp` module renamed to `OwinAppFunc` module with `fromOwinApp` and `fromOwinMonad` members
* New dependency on [F#x](http://www.nuget.org/packages/FSharpx.Core/) `Async` and `Lens` modules

### New in 0.4.0 - (Released 2014/07/21)
* Added OwinRailway programming model.

### New in 0.3.2 - (Released 2014/07/10)
* Fixed bug in setting headers from System.Net.Http types.

### New in 0.3.1 - (Released 2014/05/28)
* Fixed bug in mapping System.Net.Http types.

### New in 0.3.0 - (Released 2014/05/27)
* Added mapping to System.Net.Http types.

### New in 0.2.0 - (Released 2014/05/26)
* Added additional members through which to retrieve the request URI and the base URI.

### New in 0.1.131128 - (Released 2013/11/28)
* First official release.
