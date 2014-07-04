namespace FS2048

open System

module AutoConsole =
    let moves = [FS2048.Game.Left; FS2048.Game.Right; FS2048.Game.Up; FS2048.Game.Down]
    let randomMove =
        let r = new System.Random()
        let count = List.length moves
        fun () ->
            let i = r.Next(count)
            List.nth moves i

    let rec getMove (g:Game.State) =
        Game.displayGame g
        let move = randomMove ()
        printfn " move  = %A" move
        move