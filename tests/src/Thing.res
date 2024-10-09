type updatableOptions = LeaveUnchanged

type updatableOptionsNullable = UnsetValue | ...updatableOptions

@gql.inputUnion
type updatableNullableString = UpdateValue(string) | ...updatableOptionsNullable

@gql.inputUnion
type updatableString = UpdateValue(string) | ...updatableOptions

@gql.inputUnion
type updatableNullableBool = UpdateValue(bool) | ...updatableOptionsNullable

@gql.inputUnion
type updatableBool = UpdateValue(bool) | ...updatableOptions

@gql.inputUnion
type updatableNullableInt = UpdateValue(int) | ...updatableOptionsNullable

@gql.inputUnion
type updatableInt = UpdateValue(int) | ...updatableOptions

@gql.inputUnion
type updatableNullableFloat = UpdateValue(float) | ...updatableOptionsNullable

@gql.inputUnion
type updatableFloat = UpdateValue(float) | ...updatableOptions

@gql.inputObject
type updateThingInput = {
  name: updatableString,
  age: updatableInt,
  favoriteColor: updatableNullableString,
  isAdmin: updatableNullableBool,
  height: updatableNullableFloat,
}

@gql.type
type thing = {
  @gql.field
  id: string,
  @gql.field
  name: string,
  @gql.field
  age: int,
  @gql.field
  favoriteColor: option<string>,
  @gql.field
  isAdmin: option<bool>,
  @gql.field
  height: option<float>,
}

@gql.field
let updateThing = (_: Mutation.mutation, ~thingId: ResGraph.id, ~input: updateThingInput) => {
  let currentThing: thing = {
    id: thingId->ResGraph.idToString,
    name: "Test User",
    age: 35,
    favoriteColor: Some("Blue"),
    isAdmin: Some(true),
    height: Some(1.8),
  }

  let newThing: thing = {
    ...currentThing,
    name: switch input.name {
    | UpdateValue(name) => name
    | LeaveUnchanged => currentThing.name
    },
    age: switch input.age {
    | UpdateValue(age) => age
    | LeaveUnchanged => currentThing.age
    },
    favoriteColor: switch input.favoriteColor {
    | UpdateValue(favoriteColor) => Some(favoriteColor)
    | LeaveUnchanged => currentThing.favoriteColor
    | UnsetValue => None
    },
    isAdmin: switch input.isAdmin {
    | UpdateValue(isAdmin) => Some(isAdmin)
    | LeaveUnchanged => currentThing.isAdmin
    | UnsetValue => None
    },
    height: switch input.height {
    | UpdateValue(height) => Some(height)
    | LeaveUnchanged => currentThing.height
    | UnsetValue => None
    },
  }

  Some(newThing)
}
