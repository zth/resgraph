# Developer Experience

DX is a top priority for ResGraph. Therefore, ResGraph comes with its own dedicated tooling.

## Custom VSCode extension and LSP

By default, we ship both a VSCode extension and an LSP. The LSP will:

- Run ResGraph in watch mode automatically (so you don't need to run ResGraph in a terminal yourself)
- Report errors, provide hovers, autocomplete, and more
- Provide snippets for common actions like adding new mutations, query fields and connections

You can install the [VSCode extension by following this link](https://marketplace.visualstudio.com/items?itemName=GabrielNordeborn.vscode-resgraph).

### Running the LSP in other editors

You can run the LSP yourself easily. It runs in `stdio` mode, and you start it by doing this:

```bash
npx resgraph lsp /path/to/project/folder/running/resgraph
```
