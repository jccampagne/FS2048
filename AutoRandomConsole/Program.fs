open FS2048

[<EntryPoint>]
let main argv = 
    let g = Game.init ()
                |> Game.setRandomCell
                |> Game.setRandomCell
                |> Game.setRandomCell
    //let player = AutoMaxConsole.getMove
    //let player = AutoMax2Console.getMove
    let player = Auto3Console.getMove
    let endGame = Game.loop g player
    Game.displayGame endGame
    printfn "%d, %d, %d" ((Game.biggestCell endGame) / 1<Game.V>) (endGame.score / 1<Game.P>) (endGame.iter)
    0

