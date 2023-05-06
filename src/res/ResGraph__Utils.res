module Base64 = {
  @module("./graphqlRelayConnections.mjs")
  external encode: string => string = "encodeBase64"

  @module("./graphqlRelayConnections.mjs")
  external decode: string => string = "decodeBase64"
}
