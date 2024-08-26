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

For now, the compat `Envelop` plugin doesn't ship with ResGrah. Instead you can copy paste it into your project from below:

```typescript
// resgraphCompatPlugin.ts
import { Plugin } from "@envelop/core";
import { GraphQLSchema } from "graphql";

export const resgraphCompatPlugin = (): Plugin => {
  return {
    onSchemaChange({ schema: s, replaceSchema }) {
      const schema: GraphQLSchema = s;
      const unwrapResolverSource = (src: unknown) => {
        if (typeof src === "object" && src != null) {
          if ("_0" in src) {
            return src["_0"];
          }
          if ("VAL" in src) {
            return src["VAL"];
          }
        }

        return src;
      };

      const newSchema = new GraphQLSchema({
        ...schema.toConfig(),
        types: Object.values(schema.getTypeMap()).map((type) => {
          if ("getFields" in type) {
            const fields = type.getFields();
            Object.keys(fields).forEach((fieldName) => {
              const field = fields[fieldName];
              const defaultResolver = (source: any) => source[fieldName];
              const originalResolver =
                "resolve" in field
                  ? field.resolve ?? defaultResolver
                  : defaultResolver;

              if ("resolve" in field) {
                field.resolve = (source, args, context, info) => {
                  const src = unwrapResolverSource(source);
                  return originalResolver(src, args, context, info);
                };
              }
            });
          }
          return type;
        }),
      });

      replaceSchema(newSchema);
    },
  };
};
```

Finally, make sure you add the plugin to your `Envelop` setup:

```typescript
import { envelop, useEngine, useSchema } from "@envelop/core";
import { resgraphCompatPlugin } from "./resgraphCompatPlugin";

export const getEnveloped = envelop({
  plugins: [useSchema(schema), resgraphCompatPlugin(), useEngine(GraphQLJs)],
});
```

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
