# FsTsGen
This is a non-invasive tool for generating TypeScript DTOs from a .NET assembly with particular attention to F# records and discriminated unions.



```shell
USAGE: FsTsGen.exe [--help] --inputassembly <string> --outputdir <string> [--generatebarrel <bool>]

OPTIONS:

    --inputassembly, -in <string>
                          specify the generation source assembly (DLL)
    --outputdir, -out <string>
                          specify the output directory
    --generatebarrel <bool>
                          whether to generate a barrel (index.ts) - defaults to true
    --help                display this list of options.
```