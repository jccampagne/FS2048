open FS2048

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let g = Game.init ()
                |> Game.setRandomCell
                |> Game.setRandomCell
                |> Game.setRandomCell
    let player = ConsolePlay.getMove
    let endGame = Game.loop g player
    printfn "%d, %d" ((Game.biggestCell endGame) / 1<Game.V>) (endGame.score / 1<Game.P>)
    0

