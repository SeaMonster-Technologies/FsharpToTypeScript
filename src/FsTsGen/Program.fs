namespace FsTsGen

open FsTsGen.Generation

module Program =

    open Argu

    type CliArguments =
        | [<Mandatory; AltCommandLine("-in")>] InputAssembly of string
        | [<Mandatory; AltCommandLine("-out")>] OutputDir of string
        | GenerateBarrel of bool
        | [<AltCommandLine("-f")>] Force of bool
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | InputAssembly _ -> "specify the generation source assembly (DLL)"
                | OutputDir _ -> "specify the output directory"
                | GenerateBarrel _ -> "whether to generate a barrel (index.ts) - defaults to true"
                | Force _ -> "force generation despite warnings"

    [<EntryPoint>]
    let main args =
        let parser = ArgumentParser.Create<CliArguments>(programName = "FsTsGen.exe")

        let results = parser.Parse args

        let inputAssembly = results.GetResult InputAssembly
        let outputDir = results.GetResult OutputDir
        let genBarrel = results.GetResult(GenerateBarrel, defaultValue = true)
        let force = results.GetResult(Force, defaultValue = false)

        printfn "Running typescript generation"

        let cfg =
            { InputAssembly = inputAssembly
              OutputDir = outputDir
              GenerateBarrel = genBarrel
              Force = force }

        generateFrom cfg |> Async.RunSynchronously
