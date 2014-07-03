namespace FS2048


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
                  score : Score}

    type Move = Up | Down | Left | Right

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
            match direction with
             Up ->
                for r in 0..2 do
                    for c in 0..3 do
                        let u = b.[r].[c]
                        let v = b.[r+1].[c]
                        let (x, y, points) = merge u v
                        b.[r]  .[c] <- x
                        b.[r+1].[c] <- y
                        pointsAcc := !pointsAcc + points
            | Down  ->
                for r in 3..-1..1 do
                    for c in 0..3 do
                        let u = b.[r].[c]
                        let v = b.[r-1].[c]
                        let (x, y, points) = merge u v
                        b.[r].[c]   <- x
                        b.[r-1].[c] <- y
                        pointsAcc := !pointsAcc + points
            | Left  ->
                for r in 0..3 do
                    for c in 0..2 do
                        let u = b.[r].[c]
                        let v = b.[r].[c+1]
                        let (x, y, points) = merge u v
                        b.[r].[c]   <- x
                        b.[r].[c+1] <- y
                        pointsAcc := !pointsAcc + points
            | Right ->
                for r in 0..3 do
                    for c in 3..-1..1 do
                        let u = b.[r].[c]
                        let v = b.[r].[c-1]
                        let (x, y, points) = merge u v
                        b.[r].[c]   <- x
                        b.[r].[c-1] <- y
                        pointsAcc := !pointsAcc + points
            )
        slide g direction
        {g with score = g.score + !pointsAcc}

    let hasEmptyCell (g:State) =
        let findRow row =
            let result = Seq.tryFind (fun cell -> cell = 0<V>) row
            match result with
                | None -> false
                | Some _ -> true
        let result = Seq.tryFind findRow g.board
        (match result with
            | None -> false
            | Some (_) -> true
        )

    let cartesian xs ys = 
        xs |> List.collect (fun x -> ys |> List.map (fun y -> (x@y)))

    let hasMergeableCell (g:State) =
        let dirs = [[Left]; [Right]; [Up]; [Down]]
        let dirs2 = cartesian dirs dirs
        let applySteps moves =
            let h = {g with board = deepCopy g.board}
            let hh = List.fold (fun s dir -> move s dir) h moves
            let hasChanged = g.score <> hh.score
            hasChanged
        let changerMove = Seq.tryFind applySteps dirs2
        changerMove <> None
