namespace FsTsGen

open System

module Templates =

    [<Literal>]
    let CaseSuffix = "Case"

    [<Literal>]
    let Result = "Result"

    let importTemplate name =
        $"import {{ %s{name} }} from \"./%s{name}\""

    let exportAllTemplate name = $"export * from \"./%s{name}\""

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
        
    let fileHeader =
        let genText = $"Generated by %s{programName} v%s{programVersion} on %O{DateTimeOffset.Now}"
        let dashes = List.replicate genText.Length "-" |> String.concat ""
        $"""// %s{dashes}
// %s{genText}
// %s{dashes}
"""