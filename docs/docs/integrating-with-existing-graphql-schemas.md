---
sidebar_position: 100
---

# Integrating with existing GraphQL schemas

Thanks to ResGraph producing its own GraphQL schema, it's pretty easy to integrate ResGraph into an existing GraphQL JS schema, via schema merging. Let's walk through how to do it.

> This guide is a work in progress, so there might be inconsistencies.

## 1. Setup ReScript and ResGraph

This does not differ anything from the existing setup instructions, except for one thing - you should replicate your `Context` type from TypeScript into ReScript as `ResGraphContext.context`. Remember that you don't need to write out the entire type if you don't want to, it's fine to just write out the parts you use.

## 2. Export the GraphQL schema

The easiest way to export your ResGraph schema to TypeScript is to add a separate file where you link your schema, and then expose that via a `.d.ts` file:

```rescript
// RescriptGraphQLSchema.res
let rescriptGraphQLSchema = ResGraphSchema.schema
```

```typescript
// RescriptGraphQLSchema.d.ts
import { GraphQLSchema } from "graphql";

export const rescriptGraphQLSchema: GraphQLSchema;
```

There, the ResGraph schema is now exposed to TS.

## 3. Merge the ResGraph schema with the existing schema

> This will assume that you're using `graphql-envelop`.
> Merge your existing schema with the ResGraph schema:

```typescript
// schema.ts
import { mergeSchemas } from "@graphql-tools/schema";

export const schema = mergeSchemas({
  schemas: [existingSchema, rescriptGraphQLSchema],
});
```

You'll also need to set up an `Envelop` plugin. This will make sure that your current schema understands all the ways ResGraph can return GraphQL types.

> Note that this is only something that needs solving when you're using ResGraph with _something else_. If all you're using is ResGraph, you don't need to set this up, it'll just work.

ResGraph ships the compatibility plugin as `resgraph/compat.mjs`:

```typescript
import { Plugin } from "@envelop/core";
import { resgraphCompatPlugin } from "resgraph/compat.mjs";

const plugin: Plugin = resgraphCompatPlugin();
```

Finally, make sure you add the plugin to your `Envelop` setup:

```typescript
import { envelop, useEngine, useSchema } from "@envelop/core";
import { resgraphCompatPlugin } from "resgraph/compat.mjs";

export const getEnveloped = envelop({
  plugins: [useSchema(schema), resgraphCompatPlugin(), useEngine(GraphQLJs)],
});
```

The package also exports `unwrapResolverSource` for integrations that need the
same record/variant unwrapping without Envelop. The plugin is intentionally
small: it preserves the merged schema configuration and wraps field resolvers
so ReScript record and variant payload representations work across the schema
boundary.

## 4. Duplicate the needed types

In general, the easiest way for ResGraph and an existing schema to co-exist is to _duplicate_ types between.
So, you can go ahead and add the `query`, `mutation` and `subscription` types to your ResGraph schema if you want to use them with ResGraph.

And, any type you want to use from ResGraph that's defined in TypeScript (and vice versa), just go ahead and duplicate the definition of that too. Here's an example for a fictive `User` type:

```typescript
// This is the source type that's used throughout the GraphQL API from TypeScript
type User = {
  typename: "User";
  id: string;
  name: string;
  age: number | null;
};
```

```rescript
// This is the type duplicated to ResGraph so it's usable in ResGraph too
@gql.type
type user = {
  typename: [#User],
  id: string,
  name: string,
  age: Null.t<int>
}

```
