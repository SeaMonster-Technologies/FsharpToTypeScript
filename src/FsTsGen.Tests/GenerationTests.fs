module FsTsGen.Tests.GenerationTests

open FsTsGen.Typescript
open NUnit.Framework

[<Test>]
let ``Records are generated correctly`` () =
    let expected =
        """import { Position } from "./Position"
export interface Employee {
    employeeNum: number
    name: string
    position: Position
}"""

    let actual =
        { InterfaceName = "Employee"
          IsCaseInterface = false
          GenericArgs = [||]
          Fields =
            [| { FieldName = "employeeNum"
                 FieldType = "number"
                 Imports = [||]
                 FieldModifiers = [] }
               { FieldName = "name"
                 FieldType = "string"
                 Imports = [||]
                 FieldModifiers = [] }
               { FieldName = "position"
                 FieldType = "Position"
                 Imports = [| "Position" |]
                 FieldModifiers = [] } |] }
        |> compileInterface

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Discriminated Unions are generated correctly`` () =
    let expected =
        """
export interface Position_HrCase {
    isToby: boolean
}
export type Position =
    | 'regionalManager'
    | 'assistantToTheRegionalManager'
    | 'sales'
    | { hr: HrCase }
"""

    let actual =
        let u =
            { UnionName = "Position"
              UnionCases =
                [| { CaseName = "RegionalManager"
                     CaseType = None }
                   { CaseName = "AssistantToTheRegionalManager"
                     CaseType = None }
                   { CaseName = "Sales"; CaseType = None }
                   { CaseName = "Hr"
                     CaseType = Some "Hr" } |] }

        let is =
            [| { InterfaceName = "Position_Hr"
                 IsCaseInterface = true
                 GenericArgs = [||]
                 Fields =
                   [| { FieldName = "isToby"
                        FieldType = "boolean"
                        Imports = [||]
                        FieldModifiers = [] } |] } |]

        compileUnion (u, is)

    Assert.AreEqual(expected, actual)
