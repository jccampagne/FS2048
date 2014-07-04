namespace FS2048

open System

module AutoMaxConsole =
    let moves = [FS2048.Game.Left; FS2048.Game.Right; FS2048.Game.Up; FS2048.Game.Down]

    let evaluateMove (g:Game.State) (m:Game.Move) =
        let h = Game.move g m
        (h.score, m)

    let getMaxMove (g:Game.State) =
        let evals = List.map (evaluateMove g) moves
        let sorted = List.rev (List.sort evals)
        printfn "sorted = %A" sorted
        let (_, m) = List.head sorted
        m

    let rec getMove (g:Game.State) =
        Game.displayGame g
        let move = getMaxMove g
        printfn " move  = %A" move
        move