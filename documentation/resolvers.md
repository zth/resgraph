You can define fields for any object type or interface by defining a function.

- The source type or interface to attach to is the first _unlabelled_ argument.
- All other arguments you want for your field are labelled arguments.
- Resolvers can be sync or async.

## Arguments

- Labelled
- Can be any valid GraphQL type
- Leverage inference if you want

## Context

You can always access your current GraphQL `context` by annotating an argument with `ResGraphContext.context`, ResGraph will automatically inject your context into that argument.

The name of the argument can be anything, as long as it's annotated as (or can be inferred to be) your `ResGraphContext.context` type. Example:

```rescript
/** The currently logged in user.*/
@gql.field
let me = async (_: query, ~ctx: ResGraphContext.context) => {
  switch ctx.currentlyLoggedInUserId {
  | None => None
  | Some(userId) => await ctx.dataLoaders.userById(~userId)
  }
}
```
