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

    let (zeroBoard : Board) =
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
    
