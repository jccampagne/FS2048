namespace FS2048

open System

module ConsolePlay =
    let keyToMove k =
        match k with
              System.ConsoleKey.LeftArrow  -> Some FS2048.Game.Left
            | System.ConsoleKey.DownArrow  -> Some FS2048.Game.Down
            | System.ConsoleKey.UpArrow    -> Some FS2048.Game.Up
            | System.ConsoleKey.RightArrow -> Some FS2048.Game.Right
            | _ -> None
                
    let rec getMove (g:Game.State) =
        Game.displayGame g
        let keyInfo = System.Console.ReadKey ()
        printfn " key  = %A" keyInfo.Key
        match keyToMove keyInfo.Key with
            | Some k -> k
            | None -> getMove g
