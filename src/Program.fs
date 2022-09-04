namespace FsTsGeneration

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open FsTsGeneration.Typescript

module Generation =

    type CompilationUnit = { FileName: string; Contents: string }

    type FsImpl =
        | Union of Type * TSUnion * TSInterface []
        | Record of Type * TSInterface
        static member tryWrap(t: Type) =
            if FSharpType.IsRecord t then
                let tsInterface = convertRecordType t
                Record(t, tsInterface) |> Some
            elif isUnionBase t then
                let tsUnion, tsInterfaces = convertUnionType t
                Union(t, tsUnion, tsInterfaces) |> Some
            else
                None

        static member fullName x =
            match x with
            | Union (t, _, _)
            | Record (t, _) -> t.FullName

        static member compile x =
            match x with
            | Union (t, tsUnion, tsInterfaces) ->
                { FileName = t.Name
                  Contents = compileUnion (tsUnion, tsInterfaces) }
            | Record (t, tsInterface) ->
                { FileName = t.Name
                  Contents = compileInterface tsInterface }

    let writeCompilation path compilationUnit =
        async {

            if not (Directory.Exists path) then
                Directory.CreateDirectory path |> ignore

            let fullPath = Path.Combine(path, compilationUnit.FileName)

            let fileName = $"%s{fullPath}.ts"
            File.WriteAllText(fileName, compilationUnit.Contents)
        }

    let generateFrom path =
        let asm = Assembly.LoadFrom path

        let types = asm.GetTypes()

        let asmFilter = @"SeaMonster.Domain.Process.RuleTypes"
        let destination = @"C:\tmp\ts-generation"


        types
        |> Array.filter (fun t -> t.FullName.StartsWith asmFilter)
        |> Array.choose FsImpl.tryWrap
        |> Array.map FsImpl.compile
        |> Array.map (writeCompilation destination)
        |> Async.Parallel
        |> Async.Ignore

module Program =

    [<EntryPoint>]
    let main args =

        match args with
        //        | [||] -> printfn "Please provide an assembly path"
        | _ ->
            printfn "Running typescript generation"

            Generation.generateFrom
                @"C:\_code\seamonster-services\src\SeaMonster.Domain\bin\Debug\net6.0\SeaMonster.Domain.dll"
            |> Async.RunSynchronously

        0
