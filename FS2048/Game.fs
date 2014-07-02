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
        if   a = 0<V> then b,   0<V>
        elif a = b    then a+b, 0<V>
        else               a,   b


    let wrap_merge u v wasUpdated =
        let (x,y) = merge u v
        let hasChanged = not ((u,v) = (x,y))
        let updated = wasUpdated || hasChanged
        (x, y, updated)

    let rec move (b:Board) (direction:Move) =
        let isUpdated = ref false
        match direction with
         Up ->
            for r in 0..2 do
                for c in 0..3 do
                    let u = b.[r].[c]
                    let v = b.[r+1].[c]
                    let (x, y, updt) = wrap_merge u v !isUpdated
                    b.[r]  .[c] <- x
                    b.[r+1].[c] <- y
                    if updt then move b direction
        | Down  -> failwith "move down not implemented"
        | Left  ->
            for r in 0..3 do
                for c in 0..2 do
                    let u = b.[r].[c]
                    let v = b.[r].[c+1]
                    let (x, y, updt) = wrap_merge u v !isUpdated
                    b.[r].[c]   <- x
                    b.[r].[c+1] <- y
                    if updt then move b direction
        | Right -> failwith "move down not implemented"
