open FS2048

let rec loop g =
    let move = ConsolePlay.getMove g
    match Game.play g move with
    | Game.GameOver h -> ()
    | Game.GameContinue h -> loop h


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let g = Game.init ()
                |> Game.setRandomCell
                |> Game.setRandomCell
                |> Game.setRandomCell
    loop g
    0

