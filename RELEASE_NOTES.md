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
