namespace FsTsGeneration

module Program =

    // TEMP

    let path =
        @"C:\_code\seamonster-services\src\SeaMonster.Domain\bin\Debug\net6.0\SeaMonster.Domain.dll"

    [<EntryPoint>]
    let main args =

        match args with
        //        | [||] -> printfn "Please provide an assembly path"
        | _ ->
            printfn "Running typescript generation"

            Generation.generateFrom path
            |> Async.RunSynchronously

        0
