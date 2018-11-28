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
        with static member register key map = 
            match Map.tryFind key map with
            | Some value -> value
            | None -> 0

    type Condition = {reg: string; op: int -> int -> bool; v: int}
        with static member passed cond map = cond.op (CPU.register cond.reg map) cond.v

    type Command = {reg: string; op: int -> int -> int; v: int; cond: Condition}

    let solve (input: string list) mapper = 
        let parseIntOp = function "inc" -> (+) | "dec" | _ -> (-)
        let parseBoolOp = function ">" -> (>) | "<" -> (<) | ">=" -> (>=) | "<=" -> (<=) | "==" -> (=) | "!=" | _ -> (<>)
        
        let create (l: string list) = {reg = l.[0]; op = parseIntOp l.[1]; v = Int32.Parse l.[2]; cond = {reg = l.[4]; op = parseBoolOp l.[5]; v = Int32.Parse l.[6]}}

        let step log com =
            match Condition.passed com.cond log.registers with
            | false -> log
            | true -> 
                let assignee = com.op (CPU.register com.reg log.registers) com.v
                {registers = Map.add com.reg assignee log.registers; assigns = assignee::log.assigns}

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
        | _ -> failwith "wrong day"
    
    let solver = decider part

    day |> Utils.readInput |> solver |> printfn "%A"

    0