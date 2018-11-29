module aoc

open System
open System.IO
open System.Text.RegularExpressions

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

module Day8 =
    type CPU = {registers: Map<string, int>; assigns: int list}
        with static member register key cpu = 
            match Map.tryFind key cpu.registers with
            | Some value -> value
            | None -> 0

    type Command = {reg: string; apply: int -> int; cond: int -> bool; reg2: string}

    let solve (input: string list) mapper = 
        let parseIntOp = function "inc" -> (+) | "dec" | _ -> (-)
        let parseBoolOp = function ">" -> (>) | "<" -> (<) | ">=" -> (>=) | "<=" -> (<=) | "==" -> (=) | "!=" | _ -> (<>)
        
        let create (l: string list) = {reg = l.[0]; apply = (fun rv -> (parseIntOp l.[1]) rv (Int32.Parse l.[2])); cond = (fun rv -> (parseBoolOp l.[5]) rv (Int32.Parse l.[6])); reg2 = l.[4]}

        let step cpu com =
            match com.cond (CPU.register com.reg2 cpu) with
            | false -> cpu
            | true -> com.apply (CPU.register com.reg cpu) |> (fun a -> {registers = Map.add com.reg a cpu.registers; assigns = a::cpu.assigns})

        input |> List.map (fun s -> Utils.splitBy " " s |> create) |> List.fold step {registers = Map.empty; assigns = []} |> mapper

    let solve1 (input: string list) = solve input (fun l -> l.registers |> Map.toList |> List.maxBy (fun (k, v) -> v) |> snd)

    let solve2 (input: string list) = solve input (fun l -> l.assigns |> List.max)

    let decide part =
        match part with
        | "2" -> solve2
        | "1" | _ -> solve1

module Day9 =
    type State = {depth: int; group: int}

    let solve (input: string list) = 
        let remove pattern str = Regex.Replace (str, pattern, "")

        let garbaged = input |> List.head |> remove "\!."

        let step state ch =
            match ch with
            | '{' -> {state with depth = state.depth + 1}
            | '}' -> {depth = state.depth - 1; group = state.group + state.depth}
            | _ -> state

        (garbaged |> remove "<.*?>" |> remove "," |> Seq.fold step {depth = 0; group = 0} |> (fun s -> s.group), 
            Regex.Matches (garbaged, "<.*?>") |> Seq.sumBy (fun g -> g.Length - 2))

    let solve1 (input: string list) = input |> solve |> fst

    let solve2 (input: string list) = input |> solve |> snd

    let decide part =
        match part with
        | "2" -> solve2
        | "1" | _ -> solve1

module Day10 =
    let solve (input: string list) =
        let marksLength = 256

        let swapAt i l = l |> List.splitAt i |> (fun (f, s) -> s@f)
        let reverseAt i l = l |> List.splitAt i |> (fun (f, s) -> (f |> List.rev)@s)

        let hashing (position, marks) (skip, length) =
            let next = (position + skip) % marksLength
            (next + length - 1, marks |> swapAt next |> reverseAt length |> swapAt (marksLength - next))

        input |> List.head |> Utils.splitBy "," |> List.mapi (fun i v -> (i, Int32.Parse v)) |> List.fold hashing (0, [for i in 0 .. marksLength - 1 -> i]) |> snd |> List.take 2 |> List.reduce (*)

    let solve1 input = solve input

    let solve2 input = solve input

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
        | "8" -> Day8.decide
        | "9" -> Day9.decide
        | "10" -> Day10.decide
        | _ -> failwith "wrong day"
    
    let solver = decider part

    day |> Utils.readInput |> solver |> printfn "%A"

    0