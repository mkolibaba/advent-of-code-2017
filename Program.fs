﻿module aoc

open System
open System.IO
open System.Text.RegularExpressions

module Utils =
    let readInput day = (sprintf "resources\\input-%s.txt" day) |> File.ReadLines |> List.ofSeq
    let splitBy (splitter: string) (str: string) = str.Split([|splitter|], StringSplitOptions.None) |> List.ofArray
    let joinBy (joiner: string) (l: 'a list) = String.Join(joiner, l)

module Day4 =
    let solve mapper = List.sumBy (fun s -> Utils.splitBy " " s |> List.map mapper |> fun l -> if (l |> List.distinct |> List.length = l.Length) then 1 else 0)
    let decide = function | "2" -> solve (Seq.sort >> String.Concat) | "1" | _ -> solve id

module Day6 =
    let solve (predicate: int list -> (int list -> bool)) (input: string list) =
        let banks = input |> List.map (fun s -> Utils.splitBy "	" s |> List.map Int32.Parse)
        let len = banks |> List.head |> List.length
        let rec solve' banks =
            let last = List.last banks
            let maxi, maxv = last |> List.mapi (fun i v -> i, v) |> List.maxBy snd
            let arr = [| for i in 0 .. (len - 1) -> if i = maxi then 0 else List.item i last |]

            for i in 1 .. maxv do
                let idx = (maxi + i) % len
                arr.[idx] <- arr.[idx] + 1

            arr |> List.ofArray |> (fun l -> if (List.contains l banks) then (banks.Length - List.findIndex (predicate l) banks) else solve' (banks@[l]))
        
        solve' banks
    let decide = function | "2" -> solve (fun l -> (fun li -> li = l)) | "1" | _ -> solve (fun _ -> (fun _ -> true))

module Day8 =
    type CPU = {registers: Map<string, int>; assigns: int list} with static member register key cpu = Map.tryFind key cpu.registers |> function | Some value -> value | None -> 0
    type Command = {reg: string; apply: int -> int; cond: int -> bool; reg2: string}
    let solve mapper (input: string list) = 
        let parseIntOp = function "inc" -> (+) | "dec" | _ -> (-)
        let parseBoolOp = function ">" -> (>) | "<" -> (<) | ">=" -> (>=) | "<=" -> (<=) | "==" -> (=) | "!=" | _ -> (<>)
        let create (l: string list) = {reg = l.[0]; apply = (fun rv -> (parseIntOp l.[1]) rv (Int32.Parse l.[2])); cond = (fun rv -> (parseBoolOp l.[5]) rv (Int32.Parse l.[6])); reg2 = l.[4]}
        let step cpu com =
            match com.cond (CPU.register com.reg2 cpu) with
            | false -> cpu
            | true -> com.apply (CPU.register com.reg cpu) |> (fun a -> {registers = Map.add com.reg a cpu.registers; assigns = a::cpu.assigns})

        input |> List.map (fun s -> Utils.splitBy " " s |> create) |> List.fold step {registers = Map.empty; assigns = []} |> mapper
    let decide = function | "2" -> solve (fun l -> l.assigns |> List.max) | "1" | _ -> solve (fun l -> l.registers |> Map.toList |> List.maxBy (fun (k, v) -> v) |> snd)

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

        (garbaged |> remove "<.*?>" |> remove "," |> Seq.fold step {depth = 0; group = 0} |> (fun s -> s.group), Regex.Matches (garbaged, "<.*?>") |> Seq.sumBy (fun g -> g.Length - 2))
    let decide = function | "2" -> solve >> snd | "1" | _ -> solve >> fst

module Day10 =
    type State = {position: int; marks: int list}
    let solve (input: string list) =
        let marksLength = 256
        let swapAt i l = l |> List.splitAt i |> (fun (f, s) -> s@f)
        let reverseAt i l = l |> List.splitAt i |> (fun (f, s) -> (f |> List.rev)@s)
        let hashing state (skip, length) =
            let next = (state.position + skip) % marksLength
            {position = next + length - 1; marks = state.marks |> swapAt next |> reverseAt length |> swapAt (marksLength - next)}

        input |> List.head |> Utils.splitBy "," |> List.mapi (fun i v -> (i, Int32.Parse v)) |> List.fold hashing {position = 0; marks = [for i in 0 .. marksLength - 1 -> i]}
    let solve1 input = input |> solve |> fun s -> s.marks |> List.take 2 |> List.reduce (*) 
    let solve2 input = 
        input |> List.map (fun s -> (s |> Seq.map int |> List.ofSeq) @ [17;31;73;47;23] |> List.replicate 64 |> List.reduce (@) |> Utils.joinBy ",")
        |> solve |> (fun s -> s.marks) |> List.chunkBySize 16 |> List.map (List.reduce (^^^) >> sprintf "%02x") |> Utils.joinBy "" |> printfn "%A" 
        0 // answer will be printed out
    let decide = function | "2" -> solve2 | "1" | _ -> solve1

module Day11 =
    type State = {position: int * int * int; max: int} with static member distance (x: int, y: int, z: int) = (Math.Abs x + Math.Abs y + Math.Abs z) / 2
    let solve (input: string list) =
        let step {position = (x, y, z); max = max} direction =
            let next =
                match direction with
                | "n" -> (x, y + 1, z - 1)
                | "ne" -> (x + 1, y, z - 1)
                | "se" -> (x + 1, y - 1, z)
                | "s" -> (x, y - 1, z + 1)
                | "sw" -> (x - 1, y, z + 1)
                | "nw" -> (x - 1, y + 1, z)
                | _ -> (x, y, z)
            next |> State.distance |> function | d when d > max -> {position = next; max = d} | _ -> {position = next; max = max}

        input |> List.head |> Utils.splitBy "," |> List.fold step {position = (0, 0, 0); max = 0}
    let decide = function | "2" -> solve >> (fun s -> s.max) | "1" | _ -> solve >> (fun s -> State.distance s.position)

module Day12 =
    let toGraph = 
        List.map (fun s -> 
            let group = Utils.splitBy " <-> " s
            let value = group |> List.head |> Int32.Parse
            let edges = group |> List.last |> Utils.splitBy ", " |> List.map Int32.Parse
            (value, edges))
    
    let rec dfs discovered n graph = [
            let v, e = List.item n graph
            yield v
            let currentDiscovered = Set.add v discovered
            let undiscovered = set e - currentDiscovered
            for ud in undiscovered do yield! dfs currentDiscovered ud graph
        ]

    let solve1 = toGraph >> dfs Set.empty 0 >> List.distinct >> List.length

    let solve2 input = 
        let graph = toGraph input

        let rec loop groups = 
            let flatgroups = if List.isEmpty groups then List.empty else groups |> List.reduce (@)
            let left = graph |> List.filter (fun (v, _) -> List.contains v flatgroups |> not)
            if List.isEmpty left then groups else loop (groups@[dfs Set.empty (left |> List.head |> fst) graph])

        loop List.empty |> List.length

    let decide = function | "2" -> solve2 | "1" | _ -> solve1

module Day13 =
    let solve1 = List.map (Utils.splitBy ": ") >> List.map (fun l -> (Int32.Parse l.[0], Int32.Parse l.[1])) >> List.filter (fun (f, s) -> f % (s * 2 - 2) = 0) >> List.sumBy (fun (f, s) -> f * s)
    let solve2 input = 0
    let decide = function | "2" -> solve2 | "1" | _ -> solve1

[<EntryPoint>]
let main argv =
    let day = argv |> Array.head
    let part = argv |> Array.tail |> Array.head

    let decider day = 
        match day with
        | "4" -> Day4.decide
        | "6" -> Day6.decide
        | "8" -> Day8.decide
        | "9" -> Day9.decide
        | "10" -> Day10.decide
        | "11" -> Day11.decide
        | "12" -> Day12.decide
        | "13" -> Day13.decide
        | _ -> failwith "wrong day"
    
    let solver = part |> (decider day)

    day |> Utils.readInput |> solver |> printfn "%A"

    0