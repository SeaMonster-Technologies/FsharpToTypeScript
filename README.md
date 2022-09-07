# FsTsGen
This is a non-invasive tool for generating TypeScript DTOs from a .NET assembly with particular attention to F# records and discriminated unions.

## Usage

Install `FsTsGen` as a dotnet tool:
```shell
dotnet tool install FsTsGen
```

Run as follows:

```shell
USAGE: dotnet fsTsGen [--help] --inputassembly <string> --outputdir <string> [--generatebarrel <bool>]

OPTIONS:

    --inputassembly, -in <string>
                          specify the generation source assembly (DLL)
    --outputdir, -out <string>
                          specify the output directory
    --generatebarrel <bool>
                          whether to generate a barrel (index.ts) - defaults to true
    --help                display this list of options.
```

## Motivation

Generating types for an API client has the benefit of keeping clients synchronized with API changes. 

There are tools available, like [NSwag](https://github.com/RicoSuter/NSwag) and [TypeGen](https://github.com/jburzynski/TypeGenDocs/blob/master/source/overview.rst) that exist for this purpose, however they do not typically handle F# types particularly well. This is unfortunate, since the F# and TypeScript type systems have many parallels - Union types for example.

This tool aims to cater to APIs written in F# that returns F# types (serialized, of course), consumed by a TypeScript client.

## Non-Invasiveness

One of the goals of this tool was to not require any extra dependencies or attributes within the DTO class library. Therefore, this tool **operates on the compiled DTO assembly**.   

## Notes on Serialization

The TypeScript interface/union shapes will obviously depend on serialization rules on the API side. The initial version of this tool is opinionated about these rules.

It is recommended to use the serialization extensions in [FSharp.SystemTextJson](https://github.com/Tarmil/FSharp.SystemTextJson) with the following flags:

```f#
JsonUnionEncoding.ExternalTag
||| JsonUnionEncoding.UnwrapFieldlessTags
||| JsonUnionEncoding.UnwrapOption
||| JsonUnionEncoding.UnwrapRecordCases
```

Eventually, the hope is that this tool can be configured to handle a variety of serialization options.

## Next Up (in no particular order)
- [x] Initial version
- [ ] Add unit tests
- [ ] Add namespace/module filtering
- [ ] Add Changelog
- [ ] Support various serialization options