import {GraphQLSchema} from "graphql";

export const unwrapResolverSource = source => {
  if (typeof source !== "object" || source === null) return source;
  if ("_0" in source) return source._0;
  if ("VAL" in source) return source.VAL;
  return source;
};

export const resgraphCompatPlugin = () => ({
  onSchemaChange({schema, replaceSchema}) {
    const types = Object.values(schema.getTypeMap());

    for (const type of types) {
      if (!("getFields" in type)) continue;

      for (const [fieldName, field] of Object.entries(type.getFields())) {
        const defaultResolver = source => source?.[fieldName];
        const originalResolver = field.resolve ?? defaultResolver;
        field.resolve = (source, args, context, info) =>
          originalResolver(unwrapResolverSource(source), args, context, info);
      }
    }

    replaceSchema(new GraphQLSchema({...schema.toConfig(), types}));
  },
});
