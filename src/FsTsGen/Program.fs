namespace FsTsGen

open FsTsGen.Generation

module Program =

    open Argu
    
    type CliArguments =
        | [<Mandatory; AltCommandLine("-in")>] InputAssembly of string
        | [<Mandatory; AltCommandLine("-out")>] OutputDir of string
        | GenerateBarrel of bool
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | InputAssembly _ -> "specify the generation source assembly (DLL)"
                | OutputDir _ -> "specify the output directory"
                | GenerateBarrel _ -> "whether to generate a barrel (index.ts) - defaults to true"

    [<EntryPoint>]
    let main args =
        let parser = ArgumentParser.Create<CliArguments>(programName = "FsTsGen.exe")

        let results = parser.Parse args
        
        let inputAssembly = results.GetResult InputAssembly
        let outputDir = results.GetResult OutputDir
        let genBarrel = results.GetResult (GenerateBarrel, defaultValue = false)
        
        match args with
        //        | [||] -> printfn "Please provide an assembly path"
        | _ ->
            printfn "Running typescript generation"
            
            let cfg =
                { InputAssembly = inputAssembly
                  OutputDir = outputDir
                  GenerateBarrel = genBarrel }

            generateFrom cfg
            |> Async.RunSynchronously

        0
