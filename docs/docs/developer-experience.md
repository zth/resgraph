# Developer Experience

DX is a top priority for ResGraph. Therefore, ResGraph comes with its own dedicated tooling.

## Custom VSCode extension and LSP

By default, we ship both a VSCode extension and an LSP. The LSP will:

- Run ResGraph in watch mode automatically (so you don't need to run ResGraph in a terminal yourself)
- Report errors, provide hovers, autocomplete, and more
- Provide snippets for common actions like adding new mutations, query fields and connections

You can install the [VSCode extension by following this link](https://marketplace.visualstudio.com/items?itemName=GabrielNordeborn.vscode-resgraph).

## CLI tools

ResGraph also ships with a small CLI tools namespace for schema-aware inspection from the terminal.

### `resgraph tools find-definition`

Use `find-definition` to resolve a GraphQL type or field path back to the ReScript source that defines it.

```bash
npx resgraph tools find-definition Query.currentTime
```

This command expects either:

- `TypeName`
- `TypeName.fieldName`

For example:

```bash
npx resgraph tools find-definition User
npx resgraph tools find-definition User.name
```

The command reads the generated ResGraph state, so you need to have run `resgraph build` or `resgraph watch` first.

The default output is plain text:

```text
path: Query.currentTime
kind: resolver
file: ./src/schema/GraphQLSchema.res
range: 18:5-18:16
```

The returned data includes:

- The file where the definition lives
- The source range
- The kind of definition, such as `resolver`, `exposedField`, `objectType`, `interface`, `enum`, `union`, `inputObject`, `inputUnion`, or `scalar`

If you need machine-readable output, pass `--json`:

```bash
npx resgraph tools find-definition User.name --json
```

```json
{
  "path": "User.name",
  "kind": "exposedField",
  "file": "./src/schema/GraphQLSchema.res",
  "range": {
    "start": {"line": 2, "column": 17},
    "end": {"line": 2, "column": 40}
  }
}
```

### Running the LSP in other editors

You can run the LSP yourself easily. It runs in `stdio` mode, and you start it by doing this:

```bash
npx resgraph lsp /path/to/project/folder/running/resgraph
```
