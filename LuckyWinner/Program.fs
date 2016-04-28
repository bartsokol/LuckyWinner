let fileNotFound = "Provided file does not exist."
let incorrectNumber = "Provided number is invalid."

let getRandomNotInList list generator =
    Seq.initInfinite (fun _ -> generator ())
    |> Seq.skipWhile (fun x -> List.exists (fun l -> l = x) list)
    |> Seq.head

let rec uniqueRandom itemGenerator (current : list<'a>) number =
    match current.Length with
    | x when x < number -> uniqueRandom itemGenerator (List.concat [current; [(getRandomNotInList current itemGenerator)]]) number
    | number -> current

let getRandomRowNumbers lineCount amount =
    let r = System.Random()
    uniqueRandom (fun () -> r.Next(0, lineCount)) [] amount

let proceed filename number =
    let lines = System.IO.File.ReadAllLines(filename)
    printfn "Selecting %A lucky winners out of %A" number lines.Length
    match lines.Length with
    | 0 -> printfn "File is empty."
    | count when count <= number -> printfn "Not enough data. Expected more than %i lines, got %i lines." number count
    | count -> (getRandomRowNumbers count number)
               |> Seq.map (fun ind -> lines.[ind])
               |> Seq.iter (printfn "* %s")

let (|FileExists|_|) filename =
    match System.IO.File.Exists(filename) with
    | true -> Some filename
    | false -> fileNotFound |> (printfn "%s"); None

let (|Number|_|) number =
    match System.Int32.TryParse(number) with
    | (true,int) -> Some(int)
    | _ -> incorrectNumber |> (printfn "%s"); None

let (|FileAndCount|_|) args =
    match args with
    | [|FileExists filename; Number number|] -> Some (filename, number)
    | _ -> None

[<EntryPoint>]
let main argv = 
    match argv with
    | FileAndCount (filename, number) -> proceed filename number
    | _ -> printfn "Usage: LuckyWiner fileName numberOfLuckyWinners"
    0