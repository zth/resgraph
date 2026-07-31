---
sidebar_position: 13
---

# Directives

ResGraph supports custom directive definitions and type-system directive
applications. Directives are schema metadata: declaring one does not wrap a
resolver or give it runtime behavior automatically.

## Define a directive

Use a record when the directive has arguments. Record fields become directive
arguments and use the normal ResGraph input-type mapping.

```rescript
/** Caching metadata consumed by an explicit schema transform. */
@gql.directive({
  locations: ["OBJECT", "FIELD_DEFINITION"],
  repeatable: false,
})
type cacheControl = {
  /** Maximum cache lifetime in seconds. */
  @gql.default(60)
  maxAge: int,
  scope: option<string>,
  @deprecated("Use scope instead.")
  legacyScope: option<string>,
}
```

This emits:

```graphql
"""Caching metadata consumed by an explicit schema transform."""
directive @cacheControl(
  """Maximum cache lifetime in seconds."""
  maxAge: Int! = 60
  scope: String
  legacyScope: String @deprecated(reason: "Use scope instead.")
) on OBJECT | FIELD_DEFINITION
```

Use an abstract type for a directive without arguments:

```rescript
@gql.directive({locations: ["FIELD_DEFINITION"]})
type authenticated
```

`locations` accepts the standard GraphQL directive-location names. Definitions
may include executable-document locations even though ResGraph only applies
directives while building a schema. `repeatable` defaults to `false`.

## Apply a directive

Apply a directive with `@gql.annotate`. The `args` field is optional for a
directive without arguments.

```rescript
@gql.annotate({name: "cacheControl", args: {maxAge: 30}})
@gql.type
type product = {
  @gql.annotate({
    name: "cacheControl",
    args: {maxAge: 10, scope: "private"},
  })
  @gql.field
  name: string,
}
```

Arguments must be GraphQL constant values: null, booleans, integers, floats,
strings or enum values, arrays, records, and nested combinations. ResGraph
checks directive names, locations, repeatability, required and unknown
arguments, and value coercion while generating the schema.

Applications are currently supported on scalars, objects, interfaces, unions,
enums, enum values, input objects, input fields, and output fields. Schema-level
applications and resolver-argument applications are planned follow-ups.

## Repeatable directives and order

Repeat the attribute for a repeatable directive. ResGraph preserves exact
source order in generated SDL and in
`extensions.resgraph.appliedDirectives`.

```rescript
@gql.annotate({name: "tag", args: {name: "internal"}})
@gql.annotate({name: "tag", args: {name: "beta"}})
@gql.type
type account = {
  @gql.field id: ResGraph.id,
}
```

For ecosystem interoperability, generated types also expose the conventional
`extensions.directives` map consumed by GraphQL Tools' `getDirective` and
`getDirectives` helpers. Occurrences of the same directive remain ordered in
that map; use the ResGraph projection when order across different directive
names matters.

## Standard scalar specification URLs

Custom scalars can emit the standard `@specifiedBy` directive and native
`specifiedByURL` runtime property:

```rescript
@specifiedBy("https://example.com/scalars/uuid")
@gql.scalar
type uuid = string
```

## Runtime behavior

Directive declarations only describe schema metadata. Implement behavior with
an explicit GraphQL Tools schema transform, a Yoga or Envelop plugin, or normal
resolver logic. This keeps the schema contract inspectable without hiding
resolver wrapping or request-time work in an attribute.
