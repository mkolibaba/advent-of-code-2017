module aoc

open System
open System.IO

module Utils =
    let readInput day = (sprintf "resources\\input-%s.txt" day) |> File.ReadLines |> List.ofSeq
    
    let splitBy (ch: string) (str: string) = str.Split([|ch|], StringSplitOptions.None) |> List.ofArray

module Day4 =
    let solve (input: string list) mapper =
        let (||>>) (l: string list) f = (l, l.Length) |> f

        input
        |> List.sumBy (fun s -> 
            Utils.splitBy " " s
            |> List.map mapper
            ||>> fun (t, l) -> if (t |> List.distinct |> List.length = l) then 1 else 0)

    let solve1 (input: string list) = solve input id
            
    let solve2 (input: string list) = solve input (Seq.sort >> String.Concat)

    let decide part =
        match part with
        | "2" -> solve2
        | "1" | _ -> solve1

[<EntryPoint>]
let main argv =
    let day = argv |> Array.head
    let part = argv |> Array.tail |> Array.head

    let decider = 
        match day with
        | "4" -> Day4.decide
        | _ -> failwith "wrong day"
    
    let solver = decider part

    day |> Utils.readInput |> solver |> printfn "%A"

    0