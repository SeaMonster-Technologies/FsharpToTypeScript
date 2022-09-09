module FsTsGen.TSImpl

open System
open Microsoft.FSharp.Reflection
open FsTsGen.Typescript

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
        | Union (_, tsUnion, tsInterfaces) ->
            { FileName = tsUnion.UnionName |> withTSExt
              Contents = compileUnion (tsUnion, tsInterfaces) }
        | Record (_, tsInterface) ->
            { FileName = tsInterface.InterfaceName |> withTSExt
              Contents = compileInterface tsInterface }
