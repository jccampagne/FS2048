namespace FS2048
open System
open NUnit.Framework

open Game

[<TestFixture>]
type Test() = 

    [<Test>]
    member x.``Init zero board``() =
        let result = Game.zeroBoard
        let (expected : int<Game.V> [][]) =
                       [| [| 0<Game.V>; 0<Game.V>; 0<Game.V>; 0<Game.V>|]; // semicolon or not
                          [| 0<Game.V>; 0<Game.V>; 0<Game.V>; 0<Game.V>|]
                          [| 0<Game.V>; 0<Game.V>; 0<Game.V>; 0<Game.V>|]
                          [| 0<V>;      0<V>;      0<V>;      0<V>     |] // V or Game.V works
                       |]
        Assert.AreEqual (result, expected)
    
    [<Test>]
    member x.``Set test``() =
        let b = Game.zeroBoard
        let _ = Game.set b 0<R> 0<C> 2<V>
        let result = b.[0].[0]
        Assert.IsTrue ((result = 2<V>):bool)
//      Assert.IsTrue ((result = 2):bool)  // this does not compile… as expected
        Assert.AreEqual (result, 2<V>)
        Assert.AreEqual (result, 2) // works as well…?

