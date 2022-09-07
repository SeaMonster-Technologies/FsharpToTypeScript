module FsTsGeneration.Generation

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open FsTsGeneration.Typescript
open TSImpl

let writeCompilation path compilationUnit =
    async {

        if not (Directory.Exists path) then
            Directory.CreateDirectory path |> ignore

        let fullPath = Path.Combine(path, compilationUnit.FileName)

        let fileName = $"%s{fullPath}"
        File.WriteAllText(fileName, compilationUnit.Contents)
    }

let mkIndexFile names =
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

let generateFrom path =
    async {
        let asm = Assembly.LoadFrom path
        let asmFilter = @"SeaMonster.Domain.Process.RuleTypes"
        let destination = @"C:\tmp\ts-generation"

        let types = asm.GetTypes()
        //            |> Array.filter (fun t -> t.FullName.StartsWith asmFilter)

        let fsImpls = types |> Array.choose TSImpl.tryParse

        do! mkPreDefinitions destination fsImpls

        do!
            fsImpls
            |> Array.map TSImpl.name
            |> mkIndexFile
            |> writeCompilation destination

        do!
            fsImpls
            |> Array.map TSImpl.compile
            |> Array.map (writeCompilation destination)
            |> Async.Parallel
            |> Async.Ignore
    }
