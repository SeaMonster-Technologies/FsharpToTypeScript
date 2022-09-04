namespace FsTsGeneration

open System
open System.Reflection
open Microsoft.FSharp.Reflection

type TSType =
    | Number
    | String
    override x.ToString() =
        match x with
        | Number -> "number"
        | String -> "string"

module Whitespace =

    let mutable useSystemNewline = true

    let newline =
        if useSystemNewline then
            Environment.NewLine
        else
            "\n"

    let tab = "    "


module Templates =

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

    let primitiveMappings =
        [ typeof<int>.Name, Number
          typeof<string>.Name, String ]
        |> Map.ofList

    type TSField =
        { FieldName: string
          FieldType: string
          IsTypeImported: bool }

    type TSUnionCase =
        { CaseName: string
          CaseType: string option }

    type TSInterface =
        { InterfaceName: string
          Fields: TSField [] }

    type TSUnion =
        { UnionName: string
          UnionCases: TSUnionCase [] }
        
    type TSTypeModifier =
        | TSArray
        | TSNullable

    let getTypeMapping propTypeName =
        match primitiveMappings |> Map.tryFind propTypeName with
        | Some mapping -> string mapping, false
        | None -> propTypeName, true

    let convertRecordField (fieldInfo: PropertyInfo) =
        let (fType, imported), nameSuffix =
            if isList fieldInfo.PropertyType then
                let name, imported =
                    getTypeMapping
                        fieldInfo.PropertyType.GenericTypeArguments[0]
                            .Name

                ($"%s{name}[]", imported), ""
            elif isOption fieldInfo.PropertyType then
                getTypeMapping
                    fieldInfo.PropertyType.GenericTypeArguments[0]
                        .Name,
                "?"
            else
                getTypeMapping fieldInfo.PropertyType.Name, ""

        { FieldName =
            $"%s{fieldInfo.Name}%s{nameSuffix}"
            |> toLowerFirst
          FieldType = fType
          IsTypeImported = imported }

    let convertUnionCase (fieldInfo: UnionCaseInfo) =
        let fields =
            fieldInfo.GetFields()
            |> Array.map convertRecordField

        { InterfaceName = fieldInfo.Name
          Fields = fields }

    let convertRecordType (recordType: Type) =
        let fields =
            FSharpType.GetRecordFields recordType
            |> Array.map convertRecordField

        { InterfaceName = recordType.Name
          Fields = fields }

    let convertUnionType (unionType: Type) =
        let fieldInterfaces =
            FSharpType.GetUnionCases unionType
            |> Array.map convertUnionCase

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

    let formatRecordFields (fields: TSField []) =
        fields
        |> Array.map (fun f -> $"%s{f.FieldName}: %s{f.FieldType}")
        |> String.concat $"%s{Whitespace.newline}%s{Whitespace.tab}"

    let formatUnionCases (fields: TSField []) =
        fields
        |> Array.map (fun f -> $"%s{f.FieldName}: %s{f.FieldType}")

    let compileImports (fields: TSField []) =
        fields
        |> Array.choose (fun f ->
            if f.IsTypeImported then
                Some f.FieldType
            else
                None)
        |> Array.distinct
        |> Array.map Templates.importTemplate
        |> String.concat Whitespace.newline

    let compileInterfaceNoImports (tsInterface: TSInterface) =
        tsInterface.Fields
        |> formatRecordFields
        |> Templates.interfaceTemplate tsInterface.InterfaceName

    let compileInterface (tsInterface: TSInterface) =

        let compiledImports = compileImports tsInterface.Fields

        let compiledInterface = compileInterfaceNoImports tsInterface

        $"%s{compiledImports}%s{Whitespace.newline}%s{compiledInterface}"

    let compileUnion (tsUnion: TSUnion, tsInterfaces: TSInterface []) =

        let compiledInterfaces, compiledImports =
            match tsInterfaces with
            | [||] -> "", ""
            | _ ->
                tsInterfaces
                |> Array.map compileInterfaceNoImports
                |> String.concat $"%s{Whitespace.newline}%s{Whitespace.newline}",

                tsInterfaces
                |> Array.collect (fun i -> i.Fields)
                |> compileImports

        let compiledCases =
            tsUnion.UnionCases
            |> Array.map (fun c ->
                match c.CaseType with
                | Some t -> $"| %s{t}"
                | None -> $"| '%s{c.CaseName}'")
            |> String.concat $"%s{Whitespace.newline}%s{Whitespace.tab}"

        let compiledUnion =
            compiledCases
            |> Templates.unionTemplate tsUnion.UnionName

        let compiled =
            $"%s{compiledImports}%s{Whitespace.newline}%s{compiledInterfaces}%s{Whitespace.newline}%s{Whitespace.newline}%s{compiledUnion}"

        compiled
