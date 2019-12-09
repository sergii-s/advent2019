module AdventCalendar.Day4
open System

let min = 206938
let max = 679128

let toArray (number:int) =   
    let digit (x:int) = number / (Math.Pow(10., float x) |> int)
    [|
        digit 5
        digit 4 - (digit 5) * 10
        digit 3 - (digit 4) * 10
        digit 2 - (digit 3) * 10
        digit 1 - (digit 2) * 10
        digit 0 - (digit 1) * 10
    |]

let toDigit (numbers:int array) =
    numbers |> Array.mapi (fun i n -> n * (Math.Pow(10., float (numbers.Length - 1 - i)) |> int)) |> Array.sum

max |> toArray |> toDigit
let getRight (d: int array) =
    for i in 1..d.Length-1 do
        if d.[i] < d.[i-1] then d.[i] <- d.[i-1]
    d

let rec getNext (d: int array) = seq {
    let rec getNextRec i =
        if i >= 0 then 
            if d.[i] < 9 then
                d.[i] <- d.[i] + 1
                for j in i..d.Length-1 do
                    d.[j] <- d.[i]
                d |> Some
            else
                i-1 |> getNextRec
        else
            None
            
    let n = getNextRec (d.Length-1)
    if n |> Option.isSome then
        yield n.Value |> toDigit
        yield! getNext n.Value
}

let simple min max = seq {
    for i in min..max-1 do
        let d = i |> toArray
        let isIncr = d |> Array.mapi(fun i x -> i = 0 || x >= d.[i-1] ) |> Array.forall id
        let rec hasDouble count idx = seq {
            if idx<d.Length-1 then
                if d.[idx] = d.[idx+1] then
                    yield! hasDouble (count+1) (idx+1)
                else
                    yield count+1
                    yield! hasDouble 0 (idx+1)
            else
                yield count+1
        }
        let nunu = hasDouble 0 0
                   |> Seq.exists (fun s -> s=2)
        
        if isIncr && nunu then
            yield i
}
min |> toArray |> getRight |> getNext |> Seq.takeWhile(fun d -> d <= max) |> Array.ofSeq 
simple min max |> Seq.length