Given that fields and arguments will be exposed to the outside world through your schema, they can only be of types that can be transformed to valid GraphQL types.

Valid GraphQL types are:

- `string`, `array`,

### Custom scalars

If you end up in a situation where you need a field or argument to be of a type that's not a valid GraphQL type by default, you can leverage custom scalars to define a type that can be whatever you need it to be internally, and is exposed through your schema and to your client as a custom scalar. Read more about [custom scalars](#custom-scalars);
