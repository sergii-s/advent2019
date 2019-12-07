module AdventCalendar.Day3
open System
open System.IO


type Direction =
    | Up 
    | Down 
    | Left 
    | Right 

type Instruction = {
    Direction : Direction
    Steps : int
}
let parseInput (input:string) = seq { 
    let terms = input.Split(",")
    for t in terms do
        let steps = t.Substring(1) |> int
        let direction =
            match t.Substring(0, 1) with
            | "U" -> Up
            | "D" -> Down
            | "L" -> Left
            | "R" -> Right
        yield { Direction = direction; Steps = steps }
}

type Position = {
    X: int
    Y: int
}
let traceOne instruction position step = seq {
    let dx, dy =
        match instruction.Direction with
        | Up -> (0, 1)
        | Down -> (0, -1)
        | Left -> (-1, 0) 
        | Right -> (1, 0)
    for i in [1..instruction.Steps] do
        yield {X=position.X+i*dx; Y=position.Y+i*dy}, step+i
}
            
let trace instructions = seq {
    let mutable position = {X=0; Y=0}
    let mutable stepCounter = 0
    for instruction in instructions do
        let path = traceOne instruction position stepCounter
        let (lastPosition, lastCounter) = path |> Seq.last
        position <- lastPosition
        stepCounter <- lastCounter
        yield! path
}

let day31 (input:string array) =
    let test11 = input.[0]
    let test12= input.[1]
    let path1 = test11 |> parseInput |> trace |> Seq.map (fun (p,s) -> p) |> Set.ofSeq
    let path2 = test12 |> parseInput |> trace |> Seq.map (fun (p,s) -> p) |> Set.ofSeq
    path1 |> Set.intersect path2 |> Set.toSeq |> Seq.map(fun p -> Math.Abs(p.X) + Math.Abs(p.Y)) |> Seq.min


let day32 (input:string array) =
    let test11 = input.[0]
    let test12= input.[1]
    let path1 = test11 |> parseInput |> trace |> Map.ofSeq
    let path2 = test12 |> parseInput |> trace |> Map.ofSeq
    let distances = seq {
        for p in path1 do
            yield path2 |> Map.tryFind p.Key |> Option.map (fun d -> d + p.Value)
    }
    distances |> Seq.choose id |> Seq.min 
    


[|
  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  "U62,R66,U55,R34,D71,R55,D58,R83"
|] |> day32


[|
  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
|] |> day32

File.ReadLines "day3-1.txt"
    |> Array.ofSeq
    |> day32