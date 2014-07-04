open FS2048

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let g = Game.init ()
                |> Game.setRandomCell
                |> Game.setRandomCell
                |> Game.setRandomCell
    //let player = AutoMaxConsole.getMove
    let player = AutoMax2Console.getMove
    let endGame = Game.loop g player
    Game.displayGame endGame
    0

    