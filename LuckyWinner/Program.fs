let fileNotFound = "Provided file does not exist."
let incorrectNumber = "Provided number is invalid."

let getRandomNotInList list generator =
    Seq.initInfinite (fun _ -> generator ())
    |> Seq.takeWhile (fun x -> List.exists (fun l -> l = x) list)
    |> Seq.head

let rec uniqueRandom itemGenerator (current : list<'a>) number =
    match current.Length with
    | x when x < number -> uniqueRandom itemGenerator (List.concat [current; [(getRandomNotInList current itemGenerator)]]) number
    | number -> current

let getRandomRowNumbers lineCount amount =
    let r = System.Random()
    uniqueRandom (fun () -> r.Next(0, lineCount)) [] amount

let displayWinners items =
    Seq.iter (printfn "* %s") items

let proceed filename number =
    let lines = System.IO.File.ReadAllLines(filename)
    printfn "Selecting %A lucky winners out of %A" number lines.Length
    match lines.Length with
    | 0 -> printfn "File is empty."
    | count when count <= number -> printfn "Not enough data. Expected more than %A lines, got %A lines." number count
    | count -> (getRandomRowNumbers count number) |> Seq.map (fun ind -> lines.[ind]) |> displayWinners
    ()

let parse number =
    match System.Int32.TryParse(number) with
    | (true,int) -> Some(int)
    | _ -> None

let verify filename number =
    match (System.IO.File.Exists(filename), number) with
    | (true, Some num) -> proceed filename num
    | (true, None) -> incorrectNumber |> (printfn "%s")
    | (false, Some _) -> fileNotFound |> (printfn "%s")
    | (false, None) -> (fileNotFound + "\n" + incorrectNumber) |> (printfn "%s")

[<EntryPoint>]
let main argv = 
    match argv with
    | [|filename; number|] -> verify filename (parse number)
    | _ -> printfn "Usage: LuckyWiner fileName numberOfLuckyWinners"
    0
