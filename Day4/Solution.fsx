open System.Text.RegularExpressions
 
type Room = Room of string * int * string
 
let roomPattern = "(.+)-(\d+)\[(.+)\]$"
let parseRoom (input: string): Room =
    let m = Regex.Match (input, roomPattern)
    if m.Success
    then Room (m.Groups.[1].Value, int m.Groups.[2].Value, m.Groups.[3].Value)
    else failwith <| sprintf "Could not parse room from %s\n" input

let valid = "not-a-real-room-404[oarel]"
let invalid = "totally-real-room-200[decoy]"

let mostCommonChars (input: seq<char>): seq<char> =
    Seq.groupBy id input
    |> Seq.sortBy (fun (key, elements) -> (- (Seq.length elements), key))
    |> Seq.map fst
    |> Seq.filter ((<>) '-')
    |> Seq.take 5
 
let prefixMatches listOne listTwo =
    Seq.map2 (=) listOne listTwo
    |> Seq.forall id  
 
let isValidRoom (Room (name, _, checksum)) =
    let checkSumChars =
        name
        |> Seq.filter (fun c -> c <> '-')
        |> mostCommonChars
    prefixMatches (Seq.sort checkSumChars) (Seq.sort checksum)

let inputFile = @".\input.txt"

let content () = System.IO.File.ReadLines inputFile

let validRooms () =
    content ()
    |> Seq.map parseRoom
    |> Seq.filter isValidRoom

let solution () =
    validRooms()
    |> Seq.sumBy (fun (Room (_, id, _)) -> id)