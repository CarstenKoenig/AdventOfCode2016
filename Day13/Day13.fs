module Astar

type Score = int

// poor-mans priority queue
// via Sets
type Priority<'node when 'node : comparison> =
    { 
        nMap : Map<Score, Set<'node>>
        pMap : Map<'node, Score>
    }

module Priority =

    let empty()  : Priority<'node> =
        {
            nMap = Map.empty
            pMap = Map.empty
        }

    let mininmum (pq : Priority<'node>) =
        pq.nMap
        |> Map.pick (fun _ ns -> Some (Seq.head ns))

    let insert (n : 'node) (p : Score) (pq : Priority<'node>) =
        let nMap' =
            let lp = defaultArg (Map.tryFind p pq.nMap) Set.empty
            let lp' = Set.add n lp
            Map.add p lp' pq.nMap
        let pMap' =
            Map.add n p pq.pMap
        { nMap = nMap'; pMap = pMap' }

    let remove (n : 'node) (pq : Priority<'node>) =
        match pq.pMap.TryFind n with
        | None -> pq
        | Some p ->
            let nMap' =
                let lp = defaultArg (Map.tryFind p pq.nMap) Set.empty
                let lp' = Set.remove n lp
                if Set.isEmpty lp' then
                    Map.remove p pq.nMap
                else
                    Map.add p lp' pq.nMap
            let pMap' = Map.remove n pq.pMap
            { nMap = nMap'; pMap = pMap' }
                    

type Path<'node> = 'node seq

module Algorithm =

    type Config<'node when 'node : comparison> =
        {
            heuristic : 'node -> Score
            neighbours : 'node -> 'node seq
            distance : 'node -> 'node -> Score
            isGoal : 'node -> bool
        }

    type private Runtime<'node when 'node : comparison> =
        {
            heuristic : 'node -> Score
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
            distance : 'node -> 'node -> Score
            visitedNodes : Set<'node>
            openNodes : Priority<'node>
            gScores : Map<'node,Score>
            fScores : Map<'node,Score>
            cameFrom : Map<'node,'node>
        }
        member this.GScore node =
            defaultArg (this.gScores.TryFind node) System.Int32.MaxValue
        member this.FScore node =
            defaultArg (this.gScores.TryFind node) System.Int32.MaxValue


    let private initRuntime (start : 'node) (config : Config<'node>) =
        {
            heuristic = config.heuristic
            neighbours = config.neighbours
            isGoal = config.isGoal
            distance = config.distance
            visitedNodes = Set.empty
            openNodes = Priority.empty() |> Priority.insert start 0
            gScores = Map.empty |> Map.add start 0
            fScores = Map.empty |> Map.add start (config.heuristic start)
            cameFrom = Map.empty
        }


    let rec private reconstructPath' (acc : 'node list) (toNode : 'node) (runtime : Runtime<'node>) =
        match runtime.cameFrom.TryFind toNode with
        | None -> toNode :: acc
        | Some parent -> reconstructPath' (toNode :: acc) parent runtime


    let private reconstructPath (toNode : 'node) (runtime : Runtime<'node>) =
        reconstructPath' [] toNode runtime |> Seq.ofList


    let private processChild (node : 'node) (runtime : Runtime<'node>) (child : 'node)=
        let tentativeGScore = runtime.GScore node + runtime.distance node child
        let fScoreChild = tentativeGScore + runtime.heuristic child
        let open' = runtime.openNodes |> Priority.insert child fScoreChild
        let gScoreChild = runtime.GScore child
        if tentativeGScore >= gScoreChild then
            { runtime with openNodes = open' }
        else
            { runtime with
                openNodes = open'
                cameFrom = runtime.cameFrom.Add (child, node)
                gScores = runtime.gScores.Add (child, tentativeGScore)
                fScores = runtime.fScores.Add (child, fScoreChild)
            }


    let rec private runAlgorithm (runtime : Runtime<'node>) =
        let current = runtime.openNodes |> Priority.mininmum
        if runtime.isGoal current then
            runtime |> reconstructPath current
        else
            let open' = runtime.openNodes |> Priority.remove current
            let visited' = runtime.visitedNodes |> Set.add current
            let runtime' = { runtime with openNodes = open'; visitedNodes = visited' }
            let children =
                runtime.neighbours current
                |> Seq.filter (visited'.Contains >> not)
            let runtime'' =
                children
                |> Seq.fold (processChild current) runtime'
            runAlgorithm runtime''


    let aStar (start : 'node) (config : Config<'node>) =
        config
        |> initRuntime start
        |> runAlgorithm


module Maze =

    let private bits n =
        let gen n =
            if n <= 0 then None else
            Some (n % 2, n / 2)
        Seq.unfold gen n

    let private oneBits n =
        bits n
        |> Seq.filter ((<>) 0)
        |> Seq.length

    let isWall favNumber (x,y) =
        let nr = x*x + 3*x + 2*x*y + y + y*y + favNumber
        oneBits nr % 2 = 1

    let print favNumber (width, height) path =
        let onPath (x,y) =
            Seq.contains (x,y) path
        let out (x,y) =
            let onP = onPath (x,y)
            let onW = isWall favNumber (x,y)
            match (onW, onP) with
            | (true, true) -> 'X'
            | (true, false) -> '#'
            | (false, true) -> 'O'
            | (false, false) -> '.'
        let lineOut y =
            printfn "%s" (System.String [| for x in [0..width-1] do yield out (x,y) |])
        [0..height-1]
        |> Seq.iter lineOut


    let private dist (x,y) (x',y') =
        abs (x'-x) + abs (y'-y)

    
    let private validCoord (x,y) =
        x >= 0 && y >= 0

    let private neighbours favNumber (x,y) =
        [ (x-1,y); (x,y-1); (x+1,y); (x,y+1) ]
        |> Seq.filter (fun pos -> validCoord pos && not (isWall favNumber pos))

    let findPath favNumber fromPos toPos =
        let config : Algorithm.Config<_> =
            {
                heuristic = fun pos -> dist pos toPos
                neighbours = neighbours favNumber
                distance = fun _ _ -> 1
                isGoal = fun pos -> pos = toPos
            }
        in config |> Algorithm.aStar fromPos


let runProblem() =
    let favNumber = 1362
    let p = Maze.findPath favNumber (1,1) (31,39)
    Maze.print favNumber (50,50) p
    printfn "it took %d steps" (Seq.length p - 1)

[<EntryPoint>]
let main argv =
    runProblem()
    0 // return an integer exit code
