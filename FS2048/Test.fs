namespace FS2048
open System
open NUnit.Framework

open Game

[<TestFixture>]
type Test() =

    // helper function
    let bindCheckBoard line (expected: Game.Board) =
        fun (result:Game.State) ->
            try
                Assert.AreEqual(expected, result.board)
                result
                with ex ->
                    printf "bindCheck failed at line %s" line
                    raise ex
    // helper function
    let bindCheckScore line (expectedScore: int<Game.P>) =
        fun (result:Game.State) ->
            try
                Assert.AreEqual(expectedScore, result.score)
                result
                with ex ->
                    printf "bindCheck failed at line %s" line
                    raise ex

    // helper function
    let bindSlide direction (g:Game.State) =
        Game.slide g direction
        g

    // helper function
    let bindMove (direction:Game.Move) (g:Game.State) =
        Game.move g direction
    
    // helper function
    let makeState b =
        {Game.board = b;
         Game.score = 0<P>
         }
    
    [<Test>]
    member x.``Init zero board``() =
        let result = Game.zeroBoard ()
        let (expected : int<Game.V> [][]) =
                       [| [| 0<Game.V>; 0<Game.V>; 0<Game.V>; 0<Game.V>|]; // semicolon or not
                          [| 0<Game.V>; 0<Game.V>; 0<Game.V>; 0<Game.V>|]
                          [| 0<Game.V>; 0<Game.V>; 0<Game.V>; 0<Game.V>|]
                          [| 0<V>;      0<V>;      0<V>;      0<V>     |] // V or Game.V works
                       |]
        Assert.AreEqual (result, expected)
    
    [<Test>]
    member x.``Set test``() =
        let b = Game.zeroBoard ()
        let _ = Game.set b 0<R> 0<C> 2<V>
        let result = b.[0].[0]
        Assert.IsTrue ((result = 2<V>):bool)
//      Assert.IsTrue ((result = 2):bool)  // this does not compile… as expected
        Assert.AreEqual (result, 2<V>)
        Assert.AreEqual (result, 2) // works as well…?


    [<Test>]
    [<ExpectedException(typeof<System.IndexOutOfRangeException>)>]
    member x.``Bounds check``() =
        let b = Game.zeroBoard ()
        Game.set b 0<R> 10<C> 4<V>
       
    [<Test>]
    member x.``Merge 2 and 2``() =
        let result = Game.merge 2<V> 2<V>
        let expected = (4<V>, 0<V>, 4<P>)
        Assert.AreEqual (expected, result)

    [<Test>]
    member x.``Merge 2 and 8``() =
        let result = Game.merge 2<V> 8<V>
        let expected = (2<V>, 8<V>, 0<P>)
        Assert.AreEqual (expected, result)

    [<Test>]
    member x.``Merge 0 and 2``() =
        let result = Game.merge 0<V> 2<V>
        let expected = (0<V>, 2<V>, 0<P>)
        Assert.AreEqual (expected, result)

    [<Test>]
    member x.``Simple Slide Left``() =
        [|
                [| 4<V>; 4<V>; 0<V>; 2<V>|]
                [| 0<V>; 0<V>; 2<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
        |]
        |> makeState
        |> bindSlide Left
        |> bindCheckBoard __LINE__
            [|
                [| 4<V>; 4<V>; 2<V>; 0<V>|]
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> ignore
                

    [<Test>]
    member x.``Simple Slide Up``() =
        [|
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 2<V>; 0<V>; 2<V>|]
                [| 0<V>; 4<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 4<V>; 8<V>|]
        |]
        |> makeState
        |> bindSlide Up
        |> bindCheckBoard __LINE__
            [|
                [| 2<V>; 2<V>; 4<V>; 2<V>|]
                [| 0<V>; 4<V>; 0<V>; 8<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> ignore

    [<Test>]
    member x.``Simple Slide Right``() =
        [|
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 2<V>; 0<V>; 2<V>|]
                [| 0<V>; 4<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 4<V>; 8<V>|]
        |]
        |> makeState
        |> bindSlide Right
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 0<V>; 0<V>; 2<V>|]
                [| 0<V>; 0<V>; 2<V>; 2<V>|]
                [| 0<V>; 0<V>; 0<V>; 4<V>|]
                [| 0<V>; 0<V>; 4<V>; 8<V>|]
            |]
        |> ignore
            
    [<Test>]
    member x.``Simple Slide Down``() =
        [|
                [| 2<V>; 0<V>; 2<V>; 0<V>|]
                [| 0<V>; 2<V>; 0<V>; 2<V>|]
                [| 0<V>; 4<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 4<V>; 8<V>|]
        |]
        |> makeState
        |> bindSlide Down
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 2<V>; 2<V>; 2<V>|]
                [| 2<V>; 4<V>; 4<V>; 8<V>|]
            |]
        |> ignore

    [<Test>]
    member x.``Multiple Slides``() =
        [|
                [| 4<V>; 4<V>; 0<V>; 2<V>|]
                [| 0<V>; 0<V>; 2<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 2<V>; 0<V>; 2<V>; 0<V>|]
        |]
        |> makeState
        |> bindSlide Right
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 4<V>; 4<V>; 2<V>|]
                [| 0<V>; 0<V>; 0<V>; 2<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 2<V>; 2<V>|]
            |]
        |> bindSlide Down
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 2<V>|]
                [| 0<V>; 0<V>; 4<V>; 2<V>|]
                [| 0<V>; 4<V>; 2<V>; 2<V>|]
            |]
        |> bindSlide Up
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 4<V>; 4<V>; 2<V>|]
                [| 0<V>; 0<V>; 2<V>; 2<V>|]
                [| 0<V>; 0<V>; 0<V>; 2<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> ignore

    [<Test>]
    member x.``Simple Move 0``() =
        [|
                [| 4<V>; 4<V>; 2<V>; 2<V>|]
                [| 4<V>; 2<V>; 2<V>; 0<V>|]
                [| 2<V>; 2<V>; 2<V>; 2<V>|]
                [| 0<V>; 2<V>; 2<V>; 4<V>|]
        |]
        |> makeState
        |> bindMove Left
        |> bindCheckBoard __LINE__
            [|
                [| 8<V>; 4<V>; 0<V>; 0<V>|]
                [| 4<V>; 4<V>; 0<V>; 0<V>|]
                [| 4<V>; 4<V>; 0<V>; 0<V>|]
                [| 4<V>; 4<V>; 0<V>; 0<V>|]
            |]
        |> ignore

    [<Test>]
    member x.``Simple Move 1``() =
        let b = Game.zeroBoard ()
        Game.set b 0<R> 0<C> 2<V>
        Game.set b 1<R> 0<C> 2<V>
        Game.set b 2<R> 0<C> 2<V>
        Game.set b 0<R> 3<C> 4<V>
        b
        |> makeState
        |> bindCheckBoard __LINE__
            [|
                [| 2<V>; 0<V>; 0<V>; 4<V>|]
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> bindMove Up
        |> bindCheckScore __LINE__ 4<P> 
        |> bindCheckBoard __LINE__
            [|
                [| 4<V>; 0<V>; 0<V>; 4<V>|]
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> bindMove Left
        |> bindCheckScore __LINE__ 12<P>
        |> bindCheckBoard __LINE__
            [|
                [| 8<V>; 0<V>; 0<V>; 0<V>|]
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> bindMove Down
        |> bindCheckScore __LINE__ 12<P>
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 8<V>; 0<V>; 0<V>; 0<V>|]
                [| 2<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> bindMove Right
        |> bindCheckScore __LINE__ 12<P>
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 8<V>|]
                [| 0<V>; 0<V>; 0<V>; 2<V>|]
            |]
        |> ignore

    [<Test>]
    member x.``Simple Move 2``() =
        let b = Game.zeroBoard ()
        Game.set b 0<R> 0<C> 2<V>
        Game.set b 1<R> 0<C> 4<V>
        Game.set b 2<R> 0<C> 2<V>
        Game.set b 2<R> 1<C> 2<V>
        Game.set b 1<R> 2<C> 4<V>
        Game.set b 0<R> 3<C> 4<V>
        b
        |> makeState
        |> bindCheckScore __LINE__ 0<P>
        |> bindCheckBoard __LINE__
            [|
                [| 2<V>; 0<V>; 0<V>; 4<V>|]
                [| 4<V>; 0<V>; 4<V>; 0<V>|]
                [| 2<V>; 2<V>; 0<V>; 0<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> bindMove Right
        |> bindCheckScore __LINE__ 12<P>
        |> bindCheckBoard __LINE__
            [|
                [| 0<V>; 0<V>; 2<V>; 4<V>|]
                [| 0<V>; 0<V>; 0<V>; 8<V>|]
                [| 0<V>; 0<V>; 0<V>; 4<V>|]
                [| 0<V>; 0<V>; 0<V>; 0<V>|]
            |]
        |> ignore

    [<Test>]
    member x.``has empty Cell``() =
        let s = makeState
                            [|
                                [| 0<V>; 0<V>; 2<V>; 4<V>|]
                                [| 0<V>; 0<V>; 0<V>; 8<V>|]
                                [| 0<V>; 0<V>; 0<V>; 4<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                            |]
        Assert.IsTrue (hasEmptyCell s)

    [<Test>]
    member x.``has no empty cell``() =
        let s = makeState
                            [|
                                [| 2<V>; 2<V>; 2<V>; 4<V>|]
                                [| 2<V>; 2<V>; 2<V>; 8<V>|]
                                [| 2<V>; 2<V>; 2<V>; 4<V>|]
                                [| 2<V>; 2<V>; 2<V>; 2<V>|]
                            |]
        Assert.IsFalse (hasEmptyCell s)

    [<Test>]
    member x.``has mergeable cell``() =
        let s = makeState
                            [|
                                [| 2<V>; 2<V>; 2<V>; 4<V>|]
                                [| 2<V>; 2<V>; 2<V>; 8<V>|]
                                [| 2<V>; 2<V>; 2<V>; 4<V>|]
                                [| 2<V>; 2<V>; 2<V>; 2<V>|]
                            |]
        Assert.IsTrue (hasMergeableCell s)

    [<Test>]
    member x.``only zeros are not mergeable``() =
        let s = makeState
                            [|
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                            |]
        Assert.IsFalse (hasMergeableCell s)

    [<Test>]
    member x.``has mergeable cells in one move``() =
        let s = makeState
                            [|
                                [| 2<V>; 0<V>; 2<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                            |]
        Assert.IsFalse (hasMergeableCell s)

    [<Test>]
    member x.``has mergeable cells in 2 moves simple``() =
        let s = makeState
                            [|
                                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 2<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                            |]
        Assert.IsTrue (hasMergeableCell s)

    [<Test>]
    member x.``has mergeable cells in 2 moves tricky``() =
        (* should be able to merge with, eg:
             - Left, Up;
             - Right, Top.
           but not with: Up, Left. This will block the game.
        *)
        let s = makeState
                            [|
                                [| 2<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                                [| 0<V>; 4<V>; 2<V>; 0<V>|]
                                [| 0<V>; 0<V>; 0<V>; 0<V>|]
                            |]
        Assert.IsTrue (hasMergeableCell s)
