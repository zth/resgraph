import type {GraphQLSchema} from "graphql";

export declare function unwrapResolverSource(source: unknown): unknown;

export interface ResGraphCompatPlugin {
  onSchemaChange(input: {
    schema: GraphQLSchema;
    replaceSchema(schema: GraphQLSchema): void;
  }): void;
}

export declare function resgraphCompatPlugin(): ResGraphCompatPlugin;
