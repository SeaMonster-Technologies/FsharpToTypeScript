module FsTsGen.Generation

open System.IO
open System.Reflection
open TSImpl

let mutable writeHeader = false

type GenerationConfig =
    { InputAssembly: string
      OutputDir: string
      GenerateBarrel: bool
      Force: bool }

let writeCompilation path compilationUnit =
    async {

        if not (Directory.Exists path) then
            Directory.CreateDirectory path |> ignore

        let fullPath = Path.Combine(path, compilationUnit.FileName)

        let fileName = $"%s{fullPath}"

        let contents =
            if writeHeader then
                $"%s{Templates.fileHeader}%s{Whitespace.newline}%s{compilationUnit.Contents}"
            else
                $"%s{compilationUnit.Contents}"

        File.WriteAllText(fileName, contents)
    }

let mkBarrel names =
    let contents =
        names
        |> Array.map Templates.exportAllTemplate
        |> String.concat Whitespace.newline

    { FileName = "index" |> withTSExt
      Contents = contents }

let mkPreDefinitions path impls =
    PreDefinitions.compile impls
    |> List.map (writeCompilation path)
    |> Async.Parallel
    |> Async.Ignore

let private loadTypes (cfg: GenerationConfig) =
    async {
        let asm = Assembly.LoadFrom cfg.InputAssembly

        let types = asm.GetTypes()

        return types |> Array.choose TSImpl.tryParse
    }

let checkForDuplicateNames tsImpls =
    let duplicates =
        tsImpls
        |> Array.groupBy TSImpl.name
        |> Array.filter (fun (_, impls) -> impls.Length > 1)
        |> Array.map fst

    duplicates

let generateFrom (cfg: GenerationConfig) =
    async {
        let! tsImpls = loadTypes cfg

        let proceed =
            match checkForDuplicateNames tsImpls with
            | [||] -> true
            | duplicates ->
                let log = duplicates |> String.concat "\n"
                printfn "Detected conflicting type names. Please use -f if you want to proceed:"
                printfn $"%s{log}"
                cfg.Force

        if proceed then
            do! mkPreDefinitions cfg.OutputDir tsImpls

            if cfg.GenerateBarrel then
                do!
                    tsImpls
                    |> Array.map TSImpl.name
                    |> mkBarrel
                    |> writeCompilation cfg.OutputDir

            do!
                tsImpls
                |> Array.map TSImpl.compile
                |> Array.map (writeCompilation cfg.OutputDir)
                |> Async.Parallel
                |> Async.Ignore

            return 0
        else
            return 1
    }
