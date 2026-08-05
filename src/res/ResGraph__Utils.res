module Base64 = {
  @module("./graphqlRelayConnections.cjs")
  external encode: string => string = "encodeBase64"

  @module("./graphqlRelayConnections.cjs")
  external decode: string => string = "decodeBase64"
}
