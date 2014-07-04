// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open FS2048


let keyToMove k =
    match k with
          System.ConsoleKey.LeftArrow  -> Some FS2048.Game.Left
        | System.ConsoleKey.DownArrow  -> Some FS2048.Game.Down
        | System.ConsoleKey.UpArrow    -> Some FS2048.Game.Up
        | System.ConsoleKey.RightArrow -> Some FS2048.Game.Right
        | _ -> None
        

let displayGame (g:Game.State) =
    let b = g.board
    for i in b do
        printf "%A\n" i
    printfn "score = %A" g.score
    printfn "---------------------------"

let rec getMove g =
    displayGame g
    let keyInfo = System.Console.ReadKey ()
    printfn " key  = %A" keyInfo.Key
    match keyToMove keyInfo.Key with
        | Some k -> k
        | None -> getMove g

let rec loop g =
    let move = getMove g
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

