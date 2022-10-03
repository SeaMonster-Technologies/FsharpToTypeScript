module FsTsGen.Tests.TSImplTests

open FsTsGen
open FsTsGen.TSImpl
open FsTsGen.Typescript
open NUnit.Framework

type Branch =
    | Scranton
    | Other of branch: Branch

type Position =
    | RegionalManager
    | AssistantToTheRegionalManager
    | Sales
    | Hr of isToby: bool

type Employee =
    { EmployeeNum: int
      Name: string
      Position: Position }

type Delivery =
    { Products: string list
      SpecialItems: string array
      Priority: int option
      Customers: string list option }

type RecordWrapper<'a> = { Field: 'a }

type DUWrapper<'a> = Field of f: 'a

[<Test>]
let ``Records are parsed correctly`` () =
    let actual = typeof<Employee> |> TSImpl.tryParse |> Option.get

    let expected =
        let i =
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

        Record(typeof<Employee>, i)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Discriminated Unions are parsed correctly`` () =
    let actual = typeof<Position> |> TSImpl.tryParse |> Option.get

    let expected =
        let u =
            { UnionName = "Position"
              UnionCases =
                [| { CaseName = "RegionalManager"
                     CaseType = None }
                   { CaseName = "AssistantToTheRegionalManager"
                     CaseType = None }
                   { CaseName = "Sales"
                     CaseType = None }
                   { CaseName = "Hr"
                     CaseType = Some "PositionHr" } |] }

        let is =
            [| { InterfaceName = "PositionHr"
                 IsCaseInterface = true
                 GenericArgs = [||]
                 Fields =
                   [| { FieldName = "isToby"
                        FieldType = "boolean"
                        Imports = [||]
                        FieldModifiers = [] } |] } |]

        Union(typeof<Position>, u, is)

    Assert.AreEqual(expected, actual)


[<Test>]
let ``Discriminated Unions with recursive reference are parsed correctly`` () =
    let actual = typeof<Branch> |> TSImpl.tryParse |> Option.get

    let expected =
        let u =
            { UnionName = "Branch"
              UnionCases =
                [| { CaseName = "Scranton"
                     CaseType = None }
                   { CaseName = "Other"
                     CaseType = Some "BranchOther" } |] }

        let is =
            [| { InterfaceName = "BranchOther"
                 IsCaseInterface = true
                 GenericArgs = [||]
                 Fields =
                   [| { FieldName = "branch"
                        FieldType = "Branch"
                        Imports = [||]
                        FieldModifiers = [] } |] } |]

        Union(typeof<Branch>, u, is)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Type modifiers are applied`` () =
    let actual = typeof<Delivery> |> TSImpl.tryParse |> Option.get

    let expected =
        let i =
            { InterfaceName = "Delivery"
              IsCaseInterface = false
              GenericArgs = [||]
              Fields =
                [| { FieldName = "products"
                     FieldType = "string"
                     Imports = [||]
                     FieldModifiers = [ TSArray ] }
                   { FieldName = "specialItems"
                     FieldType = "string"
                     Imports = [||]
                     FieldModifiers = [ TSArray ] }
                   { FieldName = "priority"
                     FieldType = "number"
                     Imports = [||]
                     FieldModifiers = [ TSNullable ] }
                   { FieldName = "customers"
                     FieldType = "string"
                     Imports = [||]
                     FieldModifiers = [ TSNullable; TSArray ] } |] }

        Record(typeof<Delivery>, i)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Generic discriminated union types are parsed`` () =
    let actual =
        typeof<DUWrapper<int>>.GetGenericTypeDefinition ()
        |> TSImpl.tryParse
        |> Option.get

    let expected =
        let u =
            { UnionName = "DUWrapper"
              UnionCases =
                [| { CaseName = "Field"
                     CaseType = Some "DUWrapperField" } |] }

        let is =
            [| { InterfaceName = "DUWrapperField"
                 IsCaseInterface = true
                 //                 GenericArgs = [| "a" |]
                 GenericArgs = [||] // TODO: This is not supported yet
                 Fields =
                   [| { FieldName = "f"
                        FieldType = "a"
                        Imports = [||]
                        FieldModifiers = [] } |] } |]

        Union(typeof<DUWrapper<int>>.GetGenericTypeDefinition (), u, is)

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Generic record types are parsed`` () =
    let actual =
        typeof<RecordWrapper<int>>.GetGenericTypeDefinition ()
        |> TSImpl.tryParse
        |> Option.get

    let expected =
        let i =
            { InterfaceName = "RecordWrapper"
              IsCaseInterface = false
              GenericArgs = [| "a" |]
              Fields =
                [| { FieldName = "field"
                     FieldType = "a"
                     Imports = [||]
                     FieldModifiers = [] } |] }

        Record(typeof<RecordWrapper<int>>.GetGenericTypeDefinition (), i)

    Assert.AreEqual(expected, actual)
