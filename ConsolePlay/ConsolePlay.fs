namespace FS2048

module ConsolePlay =
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
