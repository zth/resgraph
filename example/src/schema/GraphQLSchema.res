@gql.type
type query

@gql.type
type user = {
  ...HasNameInterface.hasName,
  @gql.field age: int,
}

module Timestamp = {
  @gql.scalar
  type t = float
}

@gql.scalar
type timestamp2 = float

@gql.field
let me = (_: query, ~onlyIfAvailable) => {
  if onlyIfAvailable {
    Some({name: "Test User", age: 35})
  } else {
    None
  }
}

@gql.field
let currentTime = (_: query) => {
  let timestamp: timestamp2 = Date.now()
  timestamp->Some
}

@gql.enum
type timestampFormat = Timestamp | HumanReadable

@gql.field
let currentTimeFloat = (_: query, ~format=Timestamp) => {
  let timestamp = Date.now()

  Some(
    switch format {
    | Timestamp => timestamp->Float.toString
    | HumanReadable => "Hello"
    },
  )
}

@gql.inputObject
type coordinates = {
  lat: float,
  lng: float,
}

@gql.inputUnion
type findShopInput =
  ById(ResGraph.id) | ByAddress({postalCode: string}) | ByCoordinates(coordinates)

@gql.field
let shop = (_: query, ~input: findShopInput) => {
  switch input {
  | ById(_id) => Some("Id")
  | ByAddress(_) => Some("Address")
  | ByCoordinates(_) => Some("Coordinates")
  }
}
