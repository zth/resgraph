// Generated by ReScript, PLEASE EDIT WITH CARE

import * as GraphqlRelayConnectionsMjs from "./graphqlRelayConnections.mjs";

function encode(prim) {
  return GraphqlRelayConnectionsMjs.encodeBase64(prim);
}

function decode(prim) {
  return GraphqlRelayConnectionsMjs.decodeBase64(prim);
}

var Base64 = {
  encode: encode,
  decode: decode
};

export {
  Base64 ,
}
/* ./graphqlRelayConnections.mjs Not a pure module */
