module FsTsGen.Generation

open System.IO
open System.Reflection
open TSImpl

type GenerationConfig =
    { InputAssembly: string
      OutputDir: string
      GenerateBarrel: bool }

let writeCompilation path compilationUnit =
    async {

        if not (Directory.Exists path) then
            Directory.CreateDirectory path |> ignore

        let fullPath = Path.Combine(path, compilationUnit.FileName)

        let fileName = $"%s{fullPath}"
        let contents = $"%s{Templates.fileHeader}%s{Whitespace.newline}%s{compilationUnit.Contents}"
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

let generateFrom (cfg: GenerationConfig) =
    async {
        let asm = Assembly.LoadFrom cfg.InputAssembly

        let types = asm.GetTypes()

        let fsImpls = types |> Array.choose TSImpl.tryParse

        do! mkPreDefinitions cfg.OutputDir fsImpls

        if cfg.GenerateBarrel then
            do!
                fsImpls
                |> Array.map TSImpl.name
                |> mkBarrel
                |> writeCompilation cfg.OutputDir

        do!
            fsImpls
            |> Array.map TSImpl.compile
            |> Array.map (writeCompilation cfg.OutputDir)
            |> Async.Parallel
            |> Async.Ignore
    }
