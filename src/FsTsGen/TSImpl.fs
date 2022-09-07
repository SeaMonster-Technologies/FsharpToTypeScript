module FsTsGeneration.TSImpl

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open FsTsGeneration.Typescript

let withTSExt f = sprintf $"%s{f}.ts"

type CompilationUnit = { FileName: string; Contents: string }

type TSImpl =
    | Union of Type * TSUnion * TSInterface []
    | Record of Type * TSInterface
    static member tryParse(t: Type) =
        if FSharpType.IsRecord t then
            let tsInterface = convertRecordType t
            Record(t, tsInterface) |> Some
        elif isUnionBase t then
            let tsUnion, tsInterfaces = convertUnionType t
            Union(t, tsUnion, tsInterfaces) |> Some
        else
            None

    static member name x =
        match x with
        | Union (_, u, _) -> u.UnionName
        | Record (_, i) -> i.InterfaceName

    static member fullName x =
        match x with
        | Union (t, _, _)
        | Record (t, _) -> t.FullName

    static member compile x =
        match x with
        | Union (t, tsUnion, tsInterfaces) ->
            { FileName = withoutGenericMangling t.Name |> withTSExt
              Contents = compileUnion (tsUnion, tsInterfaces) }
        | Record (t, tsInterface) ->
            { FileName = withoutGenericMangling t.Name |> withTSExt
              Contents = compileInterface tsInterface }
