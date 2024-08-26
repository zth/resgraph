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

@gql.type
type subscription

let wait = ms => {
  Promise.make((resolve, _) => {
    let _ = setTimeout(() => resolve(), ms)
  })
}

let makeAsyncIterator: (unit => promise<AsyncIterator.value<'value>>) => AsyncIterator.t<
  'value,
> = %raw(`function makeAsyncIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this;
    }
  }
}`)

@gql.field
let countdown = (_: subscription) => {
  let countdown = ref(10)
  let iterator = makeAsyncIterator(async () => {
    await wait(500)
    let current = countdown.contents
    countdown := current - 1

    if current > 0 {
      {
        AsyncIterator.done: false,
        value: Some(current),
      }
    } else {
      {
        AsyncIterator.done: true,
        value: Some(current),
      }
    }
  })

  iterator
}
