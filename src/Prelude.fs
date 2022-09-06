namespace FsTsGeneration

open System
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module Prelude =

    /// .NET compiles union cases into their own types that inherit from the base union type.
    /// This utility function helps identify the base union type needed for TS generation.
    let isUnionBase t =
        FSharpType.IsUnion t
        && not (FSharpType.IsUnion t.BaseType)

    let isOption (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<Microsoft.FSharp.Core.Option<_>>

    let isListOrArray (t: Type) =
        t.IsGenericType
        && (t.GetGenericTypeDefinition() = typedefof<Microsoft.FSharp.Collections.List<_>>
            || t.GetGenericTypeDefinition() = typedefof<Microsoft.FSharp.Core.array<_>>)

    let isResult (t: Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<Microsoft.FSharp.Core.Result<_, _>>

    let toLowerFirst (str: string) =
        seq {
            for i in 0 .. str.Length - 1 ->
                if i = 0 then
                    string <| Char.ToLower(str[i])
                else
                    string <| str[i]
        }
        |> String.concat ""

    let withoutGenericMangling (str: string) =
        if str.Contains '`' then
            str.Substring(0, str.IndexOf('`'))
        else
            str

module PreDefinitions =

    /// Typescript version of Result<'T, 'Err>
    let TSResult =
        """
type Result<T, Err> =
    | { ok: T }
    | { error: Err }
"""
