namespace FS2048

open System

module AutoMax2Console =
    let moves = List.map (fun x -> [x]) [FS2048.Game.Left; FS2048.Game.Right; FS2048.Game.Up; FS2048.Game.Down]
    let movesSeq = (Game.cartesian moves moves) @ moves


    let compare (sa, la) (sb, lb) =
        if   sa > sb then -1
        elif sa < sb then  1
        else 
            let na = List.length la
            let nb = List.length lb
            if   na < nb then -1
            else 1


    let evaluateMovesSeq (g:Game.State) (ml:Game.Move list) =
        let gg = Game.cloneGame g
        let h = List.fold Game.move gg ml
      //  (h.score, List.head(ml))
        (h.score, ml)

    let getMaxMove (g:Game.State) =
        let evals = List.map (evaluateMovesSeq g) movesSeq
        let sorted = List.sortWith compare evals
      //  printfn "sorted 2 =\n %A" sorted
        let (_, m) = List.head sorted
        List.head m

    let rec getMove (g:Game.State) =
        Game.displayGame g
        let move = getMaxMove g
        printfn " move  = %A" move
        printfn "---------------------------"
       // let _ = System.Console.ReadKey ()
        move
