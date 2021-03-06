﻿namespace FS2048


module Game =

    // Column
    [<Measure>]
    type C

    // Row
    [<Measure>]
    type R

    // Value
    [<Measure>]
    type V

     // Point
    [<Measure>]
    type P

    type Col = int<C>
    type Row = int<R>
    type Cval = int<V>

    type Board = int<V> [] []
    type Score = int<P>

    type State = {board : Board;
                  score : Score;
                  iter  : int;
                  random : int -> int}

    type Status =   GameOver     of State
                  | GameContinue of State

    type Move = Up | Down | Left | Right

    type Player = State -> Move

    let zeroBoard () =
         [| [| 0<V>; 0<V>; 0<V>; 0<V>|]
            [| 0<V>; 0<V>; 0<V>; 0<V>|]
            [| 0<V>; 0<V>; 0<V>; 0<V>|]
            [| 0<V>; 0<V>; 0<V>; 0<V>|]
         |]

    let deepCopy a =
        let b = Array.copy a
        for i in 0..(Array.length a - 1) do
            b.[i] <- Array.copy (a.[i])
        b

    let cloneGame g =
        {g with board = deepCopy g.board}

    let int_of_row (row:Row) = row / 1<R>
    let int_of_col (col:Col) = col / 1<C>

    let set (b:Board) (row:Row) (col:Col) (value:Cval) =
        let i = int_of_row(row)
        let j = int_of_col(col)
        b.[i].[j] <- value

    let merge a b =
        if a = b then a+b, 0<V>, ((a + b) / 1<V> * 1<P>)
        else          a,      b,                   0<P>

    let slide (g:State) (direction:Move) =
        let b = g.board
        match direction with
            Left ->
                for r in 0..3 do
                    let i = ref 0
                    while !i < 3 && b.[r].[!i] <> 0<V> do
                        i := !i + 1
                    let z = ref !i
                    while !z < 3  do
                        while !z < 3 && b.[r].[!z] = 0<V> do
                            z := (!z + 1)
                        b.[r].[!i] <- b.[r].[!z]
                        b.[r].[!z] <- 0<V>
                        i := (!i + 1)
            | Up ->
                for c in 0..3 do
                    let i = ref 0
                    while !i < 3 && b.[!i].[c] <> 0<V> do
                        i := !i + 1
                    let z = ref !i
                    while !z < 3  do
                        while !z < 3 && b.[!z].[c] = 0<V> do
                            z := (!z + 1)
                        b.[!i].[c] <- b.[!z].[c]
                        b.[!z].[c] <- 0<V>
                        i := (!i + 1)
            | Right ->
                for r in 0..3 do
                    let i = ref 3
                    while !i > 0 && b.[r].[!i] <> 0<V> do
                        i := !i - 1
                    let z = ref !i
                    while !z > 0  do
                        while !z > 0 && b.[r].[!z] = 0<V> do
                            z := (!z - 1)
                        b.[r].[!i] <- b.[r].[!z]
                        b.[r].[!z] <- 0<V>
                        i := (!i - 1)
            | Down ->
                for c in 0..3 do
                    let i = ref 3
                    while !i > 0 && b.[!i].[c] <> 0<V> do
                        i := !i - 1
                    let z = ref !i
                    while !z >0   do
                        while !z > 0 && b.[!z].[c] = 0<V> do
                            z := (!z - 1)
                        b.[!i].[c] <- b.[!z].[c]
                        b.[!z].[c] <- 0<V>
                        i := (!i - 1)

    let move (g:State) (direction:Move) =
        slide g direction
        let b = g.board
        let pointsAcc = (ref 0<P>)
        let points = (
            let move_udrl (rstart, rstep, rend, rdelta) (cstart, cstep, cend, cdelta) =
                for r in rstart..rstep..rend do
                    for c in cstart..cstep..cend do
                        let rr = r + rdelta
                        let cc = c + cdelta
                        let u = b.[r] .[c]
                        let v = b.[rr].[cc]
                        let (x, y, points) = merge u v
                        b.[r] .[c]  <- x
                        b.[rr].[cc] <- y
                        pointsAcc := !pointsAcc + points
            match direction with
            | Up    -> move_udrl (0, 1, 2, 1) (0, 1, 3, 0)
            | Down  -> move_udrl (3,-1, 1,-1) (0, 1, 3, 0)
            | Left  -> move_udrl (0, 1, 3, 0) (0, 1, 2, 1)
            | Right -> move_udrl (0, 1, 3, 0) (3,-1, 1,-1)
            )
        slide g direction
        {g with score = g.score + !pointsAcc}

    let hasFreeCells (g:State) =
        let findRow row =
            let result = Seq.tryFind (fun cell -> cell = 0<V>) row
            match result with
                | None -> false
                | Some _ -> true
        let result = Seq.tryFind findRow g.board
        match result with
            | None -> false
            | Some (_) -> true


    let biggestCell (g:State) =
        let biggest = ref 0<V>
        for r in 0..3 do
            for c in 0..3 do
                let v = g.board.[r].[c]
                if v > !biggest
                then biggest := v
                else ()
        !biggest

    let cartesian xs ys =
        xs |> List.collect (fun x -> ys |> List.map (fun y -> (x@y)))

    let hasMergeableCell (g:State) =
        let dirs = [[Left]; [Right]; [Up]; [Down]]
        let dirs2 = cartesian dirs dirs
        let mergingMoves moves =
            let cloned = cloneGame g
            let moved = List.fold (fun gAcc dir -> move gAcc dir) cloned moves
            let hasChanged = moved.score > g.score
            hasChanged
        let changerMove = Seq.tryFind mergingMoves dirs2
        changerMove <> None

    let freeCells (g:State) =
        let count = ref 0
        let freeCells = ref []
        for r in 0..3 do
            for c in 0..3 do
                if g.board.[r].[c] = 0<V>
                then freeCells := (r,c) :: !freeCells
                     count := !count + 1
                else ()
        (!count, !freeCells)

    let randomCellValue (g:State) =
        if g.random(10) < 9
        then 2<V>
        else 2<V> ////////////// not 4<V> for now

    let setRandomCell (g:State) =
        match freeCells g with
            | (0, []) -> failwith "No free cells"
            | count, cells -> let r = g.random(count)
                              let r, c = List.nth cells r
                              let v = randomCellValue g
                              g.board.[r].[c] <- v
        g
    

    let init () =
        let r = new System.Random ()
        let randomer = fun n -> r.Next(n)
        setRandomCell {board = zeroBoard ();
                       score = 0<P>;
                       random = randomer;
                       iter   = 0;
                       //random = fun n -> 0 // always return 0 temporarily, for test.
        }
    
    let isGameWon g : bool =
        biggestCell g = 2048<V>

    let play (g:State) (m:Move) =
        let gCloned = cloneGame g
        let gMoved = move gCloned m
        if gMoved.board = g.board
        then
            GameContinue gCloned
        else
            let gIncr = {gMoved with iter = gMoved.iter + 1 }
            if isGameWon gIncr
            then
                GameOver gIncr
            else
                let gCell =
                            if hasFreeCells gIncr
                            then setRandomCell gIncr
                            else gIncr
                if hasMergeableCell gCell || hasFreeCells gCell
                then GameContinue gCell
                else GameOver gCell

    let displayGame (g:State) =
        let b = g.board
        for r in b do
            for c in r do
                let s =
                    if c = 0<V> then "   _"
                    else sprintf "% 4d" (c/1<V>)
                printf " %s " s
            printf "\n"
        printfn "score = %A, iter = %d" g.score g.iter
        printfn "---------------------------"

    let rec loop (g:State) (player:Player) =
        let gg = cloneGame g
        let move = player gg
        match play g move with
            | GameOver h -> h
            | GameContinue h -> loop h player
