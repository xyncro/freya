# Freya

Functional web development in F# based on the [HTTP finite state machine](https://github.com/webmachine/webmachine/wiki) used in [web machine](https://github.com/webmachine/webmachine). Compatible with OWIN [Open Web Interface for .NET (OWIN)](http://owin.org/).

* [Read the docs](http://docs.freya.io/en/latest/)
* [Review the examples](https://github.com/freya-fs/freya.examples)

## Build status

| Platform |  BuildScript | Status of last build |
| :------ | :------: | :------: |
| **Mono** | [build.sh](https://github.com/freya-fs/freya/blob/master/build.sh) | [![Travis build status](https://travis-ci.org/freya-fs/freya.svg?branch=develop)](https://travis-ci.org/freya-fs/freya) |
| **Windows** | [build.cmd](https://github.com/freya-fs/freya/blob/master/build.cmd) | [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/hbm3c402ip371dn5/branch/master?svg=true)](https://ci.appveyor.com/project/freyafs/freya/branch/master) |

[![Issue Stats][badge-issue-stats]][link-issue-stats] [![Pull Requests Stats][badge-pr-stats]][link-issue-stats]

## Packages

| Name | Description | NuGet |
| :------ | :------ | :------: |
| Freya (meta) | Meta package referencing all other Freya packages [docs](http://docs.freya.io/en/latest/index.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.svg?style=flat)](https://www.nuget.org/packages/Freya/) |
| Freya.Core | Provides the basic abstractions on which the Freya stack is built. [docs](http://docs.freya.io/en/latest/core/index.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Core.svg?style=flat)](https://www.nuget.org/packages/Freya.Core/) |
| Freya.Lenses.Http | Provides additional Freya lenses on top of [Arachne.Http](https://www.nuget.org/packages/Arachne.Http/) [docs](http://docs.freya.io/en/latest/types-and-lenses/http.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Lenses.Http.svg?style=flat)](https://www.nuget.org/packages/Freya.Lenses.Http/) |
| Freya.Lenses.Http.Cors | Provides additional Freya lenses on top of [Arachne.Http.Cors](https://www.nuget.org/packages/Arachne.Http.Cors/) [docs](http://docs.freya.io/en/latest/types-and-lenses/http-cors.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Lenses.Http.Cors.svg?style=flat)](https://www.nuget.org/packages/Freya.Lenses.Http.Cors/) |
| Freya.Recorder | Records graph interactions for use in logging and visual debugging [docs](http://docs.freya.io/en/latest/index.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Recorder.svg?style=flat)](https://www.nuget.org/packages/Freya.Recorder/) |
| Freya.Router | Provides a routing mechanism for Freya apps [docs](http://docs.freya.io/en/latest/router/index.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Router.svg?style=flat)](https://www.nuget.org/packages/Freya.Router/) |
| Freya.Machine | Provides the Freya Machine graph infrastructure [docs](http://docs.freya.io/en/latest/machine/index.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Machine.svg?style=flat)](https://www.nuget.org/packages/Freya.Machine/) |
| Freya.Machine.Router | Provides router integration for Freya Machines [docs](http://docs.freya.io/en/latest/machine/resources.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Machine.Router.svg?style=flat)](https://www.nuget.org/packages/Freya.Machine.Router/) |
| Freya.Machine.Extensions.Http | Provides the HTTP graph extension for Freya Machine [docs](http://docs.freya.io/en/latest/machine/extensions/http.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Machine.Extensions.Http.svg?style=flat)](https://www.nuget.org/packages/Freya.Machine.Extensions.Http/) |
| Freya.Machine.Extensions.Http.Cors | Provides CORS extensions to the Freya Machine graph [docs](http://docs.freya.io/en/latest/machine/extensions/http-cors.html) | [![NuGet Status](http://img.shields.io/nuget/v/Freya.Machine.Extensions.Http.Cors.svg?style=flat)](https://www.nuget.org/packages/Freya.Machine.Extensions.Http.Cors/) |

## Questions?

* Ask questions or submit issues on the [issue tracker](https://github.com/freya-fs/freya/issues)
* Join the chat on [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/freya-fs/freya?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Maintainers

* [@kolektiv](https://github.com/kolektiv)
* [@panesofglass](https://github.com/panesofglass)

 [badge-pr-stats]: http://www.issuestats.com/github/freya-fs/freya/badge/pr
 [badge-issue-stats]: http://www.issuestats.com/github/freya-fs/freya/badge/issue
 [link-issue-stats]: http://www.issuestats.com/github/freya-fs/freya
