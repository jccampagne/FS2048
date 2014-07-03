namespace FS2048


module Game =
    
    [<Measure>]
    type C

    [<Measure>]
    type R

    [<Measure>]
    type V

    type Col = int<C>
    type Row = int<R>
    type Cval = int<V>

    type Board = int<V> [] []

    type State = {board : Board;
                  score : int}

    type Move = Up | Down | Left | Right

    let zeroBoard () =
         [| [| 0<V>; 0<V>; 0<V>; 0<V>|]
            [| 0<V>; 0<V>; 0<V>; 0<V>|]
            [| 0<V>; 0<V>; 0<V>; 0<V>|]
            [| 0<V>; 0<V>; 0<V>; 0<V>|]
         |]

    let int_of_row (row:Row) = row / 1<R>
    let int_of_col (col:Col) = col / 1<C>

    let set (b:Board) (row:Row) (col:Col) (value:Cval) =
        let i = int_of_row(row)
        let j = int_of_col(col)
        b.[i].[j] <- value
    
    let merge a b =
        if a = b    then a+b, 0<V>
        else               a,   b

    let slide (b:Board) (direction:Move) =
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
    
    let move (b:Board) (direction:Move) =
        slide b direction
        (
        match direction with
         Up ->
            for r in 0..2 do
                for c in 0..3 do
                    let u = b.[r].[c]
                    let v = b.[r+1].[c]
                    let (x, y) = merge u v
                    b.[r]  .[c] <- x
                    b.[r+1].[c] <- y
        | Down  ->
            for r in 3..-1..1 do
                for c in 0..3 do
                    let u = b.[r].[c]
                    let v = b.[r-1].[c]
                    let (x, y) = merge u v
                    b.[r].[c]   <- x
                    b.[r-1].[c] <- y
        | Left  ->
            for r in 0..3 do
                for c in 0..2 do
                    let u = b.[r].[c]
                    let v = b.[r].[c+1]
                    let (x, y) = merge u v
                    b.[r].[c]   <- x
                    b.[r].[c+1] <- y
        | Right ->
            for r in 0..3 do
                for c in 3..-1..1 do
                    let u = b.[r].[c]
                    let v = b.[r].[c-1]
                    let (x, y) = merge u v
                    b.[r].[c]   <- x
                    b.[r].[c-1] <- y
        )
        slide b direction
