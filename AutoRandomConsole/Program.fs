open FS2048

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let g = Game.init ()
                |> Game.setRandomCell
                |> Game.setRandomCell
                |> Game.setRandomCell
    let player = AutoMaxConsole.getMove
    Game.loop g player
    0

    