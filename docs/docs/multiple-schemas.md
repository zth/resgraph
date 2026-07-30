---
sidebar_position: 2
---

# Multiple schemas

A repository can build several independent ResGraph schemas from one `resgraph.json`. Schemas may belong to the same ReScript package or to different packages in a monorepo.

## Configuration

Use the named `schemas` form:

```json
{
  "defaultSchema": "public",
  "schemas": {
    "public": {
      "projectRoot": ".",
      "include": ["src/graphql/shared", "src/graphql/public"],
      "outputFolder": "src/graphql/__generated__/public",
      "contextType": "PublicContext.context",
      "dumpSchemaSdl": true
    },
    "admin": {
      "projectRoot": ".",
      "include": ["src/graphql/shared", "src/graphql/admin"],
      "exclude": ["src/graphql/admin/experimental"],
      "outputFolder": "src/graphql/__generated__/admin",
      "moduleName": "AdminSchema",
      "contextType": "AdminContext.context",
      "dumpSchemaSdl": true
    }
  }
}
```

Each schema supports:

- `projectRoot`: the directory containing its `rescript.json` or `bsconfig.json`. It defaults to the directory containing `resgraph.json`.
- `include`: source files or directories belonging to this schema. An empty or omitted array includes all project modules. Paths must stay inside `projectRoot`.
- `exclude`: optional source files or directories removed after applying `include`.
- `outputFolder`: an existing directory for generated files. Every schema must have a different output folder.
- `moduleName`: the generated schema module. It defaults from the schema name: `public` becomes `PublicSchema` and `internal-api` becomes `InternalApiSchema`.
- `contextType`: the qualified context type injected into this schema's resolvers. It defaults to `ResGraphContext.context`.
- `dumpSchemaSdl`: whether to write `schema.graphql` in this schema's output folder.

All paths are relative to the directory containing `resgraph.json`.

The generated interface helpers are also schema-specific. For example, the `public` schema emits `PublicSchema__Interface_node.res`. Use that module when a resolver explicitly returns an interface resolver type for the public schema.

## Sharing modules

Include the same directory in several schemas to share GraphQL types and fields:

```json
"include": ["src/graphql/shared", "src/graphql/public"]
```

Membership is explicit. Referencing a ReScript type from another module does not automatically include every `@gql.field` resolver declared in that module. Add the resolver module's file or directory to `include` when it should contribute fields.

ResGraph always excludes each schema's output folder from discovery, so generated interface helpers cannot feed back into the next schema build.

## Building and watching

Build or watch every configured schema:

```bash
npx resgraph build
npx resgraph watch
```

Select one schema by name:

```bash
npx resgraph build public
npx resgraph watch admin
```

A failure in one schema does not prevent the remaining schemas from generating, but the overall build exits with a non-zero status.

## Tools and editor behavior

Select the state used by `find-definition` with `--schema`:

```bash
npx resgraph tools find-definition Query.currentUser --schema public
```

ResGraph persists independent state under `lib/resgraph/<schema>.state.marshal`.

The LSP builds and reports diagnostics for every configured schema. A generated `schema.graphql` is matched to its owning output folder. For source files shared by several schemas, schema-dependent editor operations use `defaultSchema`; if it is omitted, the first configured schema is the default.

## Multiple ReScript packages

A central config can point schemas at different packages:

```json
{
  "schemas": {
    "storefront": {
      "projectRoot": "apps/storefront",
      "include": ["apps/storefront/src"],
      "outputFolder": "apps/storefront/src/generated"
    },
    "backoffice": {
      "projectRoot": "apps/backoffice",
      "include": ["apps/backoffice/src"],
      "outputFolder": "apps/backoffice/src/generated"
    }
  }
}
```

Compile each ReScript package before running ResGraph. Watch mode observes the compiler log for every distinct `projectRoot`.

## Existing configuration

The original single-schema configuration remains supported:

```json
{
  "src": "./src",
  "outputFolder": "./src/schema/__generated__"
}
```

It continues to emit `ResGraphSchema`, `Interface_*`, and `lib/.resgraphState.marshal`. In this legacy form, `src` locates the ReScript package; ResGraph scans all GraphQL modules in that package. Use the named form when source membership must define separate schemas.
