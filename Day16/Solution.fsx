let testInput = "10000"

let input = "10001001100000001"

let testTargetLength = 20

let targetLength = 272

let invert (s : string) =
    let inv c =
        if c = '0' then '1' else '0'
    Seq.map inv s
    |> Seq.rev
    |> Array.ofSeq
    |> System.String

let rec blowUp toLen (str : string) =
    if str.Length >= toLen then str.[..toLen-1] else
    let str' = str + "0" + invert str
    in blowUp toLen str'


let reduce (inp : string) =
    let sb = System.Text.StringBuilder ()
    let check c1 c2 =
        if c1 = c2 then '1' else '0'
    let mutable i = 0
    while i+1 < inp.Length do
        sb.Append (check inp.[i] inp.[i+1]) |> ignore
        i <- i+2
    sb.ToString ()

let rec checkSum (inp : string) =
    let inp' = reduce inp
    in if inp'.Length % 2 = 0 then checkSum inp' else inp'

let solve toLen inp =
    checkSum (blowUp toLen inp)