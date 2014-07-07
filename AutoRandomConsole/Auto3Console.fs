namespace FS2048

open System

module Auto3Console =
    let moves = List.map (fun x -> [x]) [FS2048.Game.Left; FS2048.Game.Right; FS2048.Game.Up; FS2048.Game.Down]
    let movesSeq = (Game.cartesian moves (Game.cartesian moves (Game.cartesian moves (Game.cartesian moves moves))))

    let compare (sa, la) (sb, lb) =
        if   sa > sb then -1
        elif sa < sb then  1
        else 
            let na = List.length la
            let nb = List.length lb
            if   na < nb then -1
            else 1

    let rec evaluateMovesSeq (g:Game.State) (movesToPlay:list<Game.Move>) (playedMovesAcc:list<Game.Move>) =
        match movesToPlay with
        | [] ->
            (g.score, List.rev playedMovesAcc)
        | m :: ms ->
            let gg = Game.cloneGame g
            match Game.play gg m with
            | Game.GameOver h     ->
                                    (h.score, List.rev (m::playedMovesAcc))
            | Game.GameContinue h ->
                if h.board = gg.board
                then (h.score, List.rev playedMovesAcc)
                else evaluateMovesSeq h ms (m::playedMovesAcc)


    let getMaxMove (g:Game.State) =
        let evals = List.map (fun ml -> let gg = Game.cloneGame g
                                        evaluateMovesSeq gg ml []) movesSeq
        let filtered = List.filter (fun (_,l) -> l <> []) evals
        let sorted = List.sortWith compare filtered
        //printfn "sorted 2 =\n %A" sorted
        let (_, m) = List.head sorted
        List.head m

    let rec getMove (g:Game.State) =
        Game.displayGame g
        let move = getMaxMove (Game.cloneGame g)
        printfn " move  = %A" move
        printfn "---------------------------"
//        let _ = System.Console.ReadKey ()
        move
