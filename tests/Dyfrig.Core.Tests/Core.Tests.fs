namespace Dyfrig.Core.Tests

open NUnit.Framework


module Tests =

    [<Test>]
    let ``definitely true`` () =
        let x = dict [ "hello", "world" ]

        Assert.True (x.Count = 1)