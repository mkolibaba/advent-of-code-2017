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

module Day6 =
    let solve (input: string list) (predicate: int list -> (int list -> bool)) =
        let i = input |> List.map (fun s -> Utils.splitBy "	" s |> List.map Int32.Parse)

        let len = i |> List.head |> List.length
        
        let rec solve' (banks: int list list) =
            let last = List.last banks
            let maxi, maxv = last |> List.mapi (fun i v -> i, v) |> List.maxBy snd

            let arr = [| for i in 0 .. (len - 1) -> if i = maxi then 0 else List.item i last |]
            
            for i in 1 .. maxv do
                let idx = (maxi + i) % len
                arr.[idx] <- arr.[idx] + 1

            let l = List.ofArray arr
            if (List.contains l banks) then (banks.Length - List.findIndex (predicate l) banks) else solve' (banks@[l])
        
        solve' i

    let solve1 (input: string list) = solve input (fun _ -> (fun _ -> true))

    let solve2 (input: string list) = solve input (fun l -> (fun li -> li = l))

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
        | "6" -> Day6.decide
        | _ -> failwith "wrong day"
    
    let solver = decider part

    day |> Utils.readInput |> solver |> printfn "%A"

    0