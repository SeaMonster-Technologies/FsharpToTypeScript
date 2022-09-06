namespace FsTsGeneration

open System
open System.Reflection
open FsTsGeneration
open Microsoft.FSharp.Reflection

type TSPrimitive =
    | Void
    | Number
    | String
    | Bool
    | Date
    override x.ToString() =
        match x with
        | Void -> "void"
        | Number -> "number"
        | String -> "string"
        | Bool -> "boolean"
        | Date -> "Date"

    static member mappings =
        [ typeof<unit>.Name, Void
          typeof<int8>.Name, Number
          typeof<int16>.Name, Number
          typeof<int32>.Name, Number
          typeof<int>.Name, Number
          typeof<int64>.Name, Number
          typeof<float>.Name, Number
          typeof<double>.Name, Number
          typeof<string>.Name, String
          typeof<Guid>.Name, String
          typeof<bool>.Name, Bool
          typeof<DateTimeOffset>.Name, Date
          typeof<DateTime>.Name, Date ]
        |> Map.ofList

type TSType =
    | Primitive of TSPrimitive
    | Result of TSType * TSType
    | Custom of string

module Whitespace =

    let mutable useSystemNewline = true

    let newline =
        if useSystemNewline then
            Environment.NewLine
        else
            "\n"

    let tab = "    "


module Templates =

    [<Literal>]
    let CaseSuffix = "Case"

    let importTemplate name =
        $"import {{ %s{name} }} from \"./%s{name}\""

    let interfaceTemplate name fields =
        $"""
export interface %s{name} {{
    %s{fields}
}}"""

    let unionTemplate name cases =
        $"""
export type %s{name} =
    %s{cases}
        """

module Typescript =

    // Case order is significant; it dictates the order in which modifiers will be applied.
    type TSFieldModifier =
        | TSArray
        | TSNullable

    type TSField =
        { FieldName: string
          FieldType: string
          Imports: string []
          FieldModifiers: TSFieldModifier list }

    type TSUnionCase =
        { CaseName: string
          CaseType: string option }

    type TSInterface =
        { InterfaceName: string
          IsCaseInterface: bool
          Fields: TSField [] }

    type TSUnion =
        { UnionName: string
          UnionCases: TSUnionCase [] }

    let toTsType propTypeName =
        match TSPrimitive.mappings |> Map.tryFind propTypeName with
        | Some mapping -> Primitive mapping
        | None -> Custom propTypeName

    let rec getTypeMapping propertyType =
        if isListOrArray propertyType then
            let typeMapping, mods = getTypeMapping propertyType.GenericTypeArguments[0]
            typeMapping, TSArray :: mods
        elif isOption propertyType then
            let typeMapping, mods = getTypeMapping propertyType.GenericTypeArguments[0]
            typeMapping, TSNullable :: mods
        elif isResult propertyType then
            let aMapping, aMods = getTypeMapping propertyType.GenericTypeArguments[0]
            let errMapping, errMods = getTypeMapping propertyType.GenericTypeArguments[1]
            Result(aMapping, errMapping), aMods @ errMods
        else
            toTsType propertyType.Name, []

    let convertRecordField parentTypeName (fieldInfo: PropertyInfo) =
        let tsType, tMods = getTypeMapping fieldInfo.PropertyType

        let rec fmtType tsType =
            match tsType with
            | Custom t ->
                t,
                if t <> parentTypeName then
                    [ t ]
                else
                    [] // Don't import types that fields that refer to their parent types
            | Primitive t -> string t, []
            | Result (a, err) ->
                let aType, aImports = fmtType a
                let errType, errImports = fmtType err
                let imports = aImports @ errImports
                $"Result<%s{aType}, %s{errType}>", imports

        let fType, imports = fmtType tsType

        { FieldName = toLowerFirst fieldInfo.Name
          FieldType = fType
          Imports = Array.ofList imports
          FieldModifiers = tMods }

    let convertUnionCase parentTypeName (fieldInfo: UnionCaseInfo) =
        let fields =
            fieldInfo.GetFields()
            |> Array.map (convertRecordField parentTypeName)

        { InterfaceName = fieldInfo.Name
          IsCaseInterface = true
          Fields = fields }

    let convertRecordType (recordType: Type) =
        let fields =
            FSharpType.GetRecordFields recordType
            |> Array.map (convertRecordField recordType.Name)

        { InterfaceName = recordType.Name
          IsCaseInterface = false
          Fields = fields }

    let convertUnionType (unionType: Type) =
        let fieldInterfaces =
            FSharpType.GetUnionCases unionType
            |> Array.map (convertUnionCase unionType.Name)

        let cases: TSUnionCase [] =
            fieldInterfaces
            |> Array.map (fun f ->
                { CaseName = f.InterfaceName
                  CaseType =
                    if Array.isEmpty f.Fields then
                        None
                    else
                        Some f.InterfaceName })

        { UnionName = unionType.Name
          UnionCases = cases },
        fieldInterfaces
        |> Array.filter (fun i -> i.Fields.Length > 0)

    let private applyFieldModifiers (f: TSField) =
        f.FieldModifiers
        |> List.sort
        |> List.fold
            (fun (fName, fType) fMod ->
                match fMod with
                | TSNullable -> fName, $"%s{fType} | null"
                | TSArray -> fName, $"%s{fType}[]")
            (f.FieldName, f.FieldType)


    let formatRecordField (f: TSField) =
        let fieldName, fieldType = applyFieldModifiers f
        $"%s{fieldName}: %s{fieldType}"

    let formatRecordFields (fields: TSField []) =
        fields
        |> Array.map formatRecordField
        |> String.concat $"%s{Whitespace.newline}%s{Whitespace.tab}"

    let formatUnionCase (case: TSUnionCase) =
        match case.CaseType with
        | Some t -> $"| {{ %s{toLowerFirst case.CaseName}: %s{t}%s{Templates.CaseSuffix} }}"
        | None -> $"| '%s{toLowerFirst case.CaseName}'"

    let compileImports (fields: TSField []) =
        fields
        |> Array.collect (fun f -> f.Imports)
        |> Array.distinct
        |> Array.map Templates.importTemplate
        |> String.concat Whitespace.newline

    let compileInterfaceNoImports (tsInterface: TSInterface) =
        let suffix =
            if tsInterface.IsCaseInterface then
                Templates.CaseSuffix
            else
                ""

        tsInterface.Fields
        |> formatRecordFields
        |> Templates.interfaceTemplate $"%s{tsInterface.InterfaceName}%s{suffix}"

    let compileInterface (tsInterface: TSInterface) =

        let compiledImports = compileImports tsInterface.Fields

        let compiledInterface = compileInterfaceNoImports tsInterface

        $"%s{compiledImports}%s{compiledInterface}"

    let compileUnion (tsUnion: TSUnion, tsInterfaces: TSInterface []) =

        let compiledInterfaces, compiledImports =
            match tsInterfaces with
            | [||] -> "", ""
            | _ ->
                let interfaces =
                    tsInterfaces
                    |> Array.map compileInterfaceNoImports
                    |> String.concat $"%s{Whitespace.newline}"

                let imports =
                    tsInterfaces
                    |> Array.collect (fun i -> i.Fields)
                    |> compileImports

                $"%s{interfaces}", $"%s{imports}"


        let compiledCases =
            tsUnion.UnionCases
            |> Array.map formatUnionCase
            |> String.concat $"%s{Whitespace.newline}%s{Whitespace.tab}"

        let compiledUnion =
            compiledCases
            |> Templates.unionTemplate tsUnion.UnionName

        let compiled = $"%s{compiledImports}%s{compiledInterfaces}%s{compiledUnion}"

        compiled
