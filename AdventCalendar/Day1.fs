module Day1
open System
open System.IO

let input = File.ReadLines "day1-1.txt"

let rec getFuelForFuel x =
    let moreFuel = x / 3 - 2
    if moreFuel > 0 then
        moreFuel + (getFuelForFuel moreFuel)
    else
        0
let modules = input |> Seq.map Int32.Parse |> Seq.map getFuelForFuel |> Seq.sum 