module FsTsGen.PreDefinitions
 
open FsTsGen.Typescript
open TSImpl

[<Literal>]
let TSResult =
    """
export type Result<T, Err> =
| { ok: T }
| { error: Err }
"""

let compile (types: TSImpl seq) =
    
    let fieldsHavePreDefinition def (fields: TSField []) =
        fields |> Array.exists (fun f -> f.Imports |> Array.contains def)
        
    let hasResult =
        types
        |> Seq.exists (
            function
            | Union (_, u, is) ->
                u.UnionCases |> Array.exists (fun c -> c.CaseType = Some Templates.Result)
                || is |> Array.exists (fun i -> i.Fields |> fieldsHavePreDefinition Templates.Result)
            | Record (_, i) -> i.Fields |> fieldsHavePreDefinition Templates.Result
        )
    
    [
        if hasResult then
            { Contents = TSResult
              FileName = "Result" |> withTSExt }
    ]