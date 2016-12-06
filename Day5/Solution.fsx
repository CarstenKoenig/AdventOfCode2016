module Day5

open System.Security.Cryptography
open System.Text

let input = @"reyedfim"

let bytes  (s:string) =  Encoding.ASCII.GetBytes(s)

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string


let stream (input : string) =
    let get n = 
        let candidate = input + n.ToString()
        bytes candidate |> md5
    Seq.initInfinite get

let startsWith5Zeros (stream : string seq) =
    stream
    |> Seq.filter (fun hash -> hash.StartsWith "00000")

let getRelevantChar (hash : string) =
    hash.[5..5]

let appendRelevantChar (password : string) (hash : string) =
    password + getRelevantChar hash

let decode (input : string) =
    stream input
    |> startsWith5Zeros
    |> Seq.take 8
    |> Seq.fold appendRelevantChar ""


let getRelevantCharAndPos (hash : string) =
    match System.Int32.TryParse hash.[5..5] with
    | (true, index) -> (index, hash.[6..6])
    | (false, index) -> (99, "#") // sureley out of range

let replaceRelevantCharWithPos (password : string) (hash : string) =
    let (pos, char) = getRelevantCharAndPos hash
    if pos < password.Length && password.[pos] = '_'
    then password.[0..pos-1] + char + password.[pos+1..]
    else password


let isDecoded (password : string) =
    not (password.Contains "_")


let takeUntil (p : 'a -> bool) (xs : 'a seq) =
    seq {
        let enum = xs.GetEnumerator()
        let mutable stop = false
        while not stop && enum.MoveNext () do
            yield enum.Current
            stop <- p enum.Current
    }


let decodeWithPos (input : string) =
    stream input
    |> startsWith5Zeros
    |> Seq.scan replaceRelevantCharWithPos "________"
    |> takeUntil isDecoded
    |> Seq.last


let day5 = 
    printfn "\n\nDay 5\n"    
        
    // printfn "Part 1: %s" (decode input)
    printfn "Part 2: %s"  (decodeWithPos input)