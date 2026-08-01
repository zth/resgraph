import {defaultFieldResolver, isIntrospectionType} from "graphql";

const compatResolver = Symbol.for("resgraph.compatResolver");

export const unwrapResolverSource = source => {
  if (typeof source !== "object" || source === null) return source;
  if ("_0" in source) return source._0;
  if ("VAL" in source) return source.VAL;
  return source;
};

export const resgraphCompatPlugin = () => ({
  onSchemaChange({schema, replaceSchema}) {
    for (const type of Object.values(schema.getTypeMap())) {
      if (isIntrospectionType(type)) continue;
      if (!("getFields" in type)) continue;

      for (const [fieldName, field] of Object.entries(type.getFields())) {
        const originalResolver = field.resolve ?? defaultFieldResolver;
        if (originalResolver[compatResolver]) continue;

        const resolver = (source, args, context, info) =>
          originalResolver(unwrapResolverSource(source), args, context, info);
        Object.defineProperty(resolver, compatResolver, {value: true});
        field.resolve = resolver;
      }
    }

    replaceSchema(schema);
  },
});
