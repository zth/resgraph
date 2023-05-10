// Modified and taken from https://github.com/epoberezkin/fast-json-stable-stringify/blob/master/index.js
export function stableStringify(data) {
  let seen = [];
  return (function stringify(node) {
    let colonSeparator = ":";
    if (node && typeof node.toJSON === "function") {
      node = node.toJSON();
    }

    if (node === undefined) return;
    if (typeof node === "number") return isFinite(node) ? "" + node : "null";
    if (typeof node !== "object") return JSON.stringify(node, undefined, "");

    let i;
    let out;
    if (Array.isArray(node)) {
      let sortedNode = node.slice().sort();
      out = "[";
      for (i = 0; i < sortedNode.length; i++) {
        if (i) out += ",";
        out += stringify(sortedNode[i]) || "null";
      }
      return out + "]";
    }

    if (node === null) return "null";

    if (seen.indexOf(node) !== -1) {
      return JSON.stringify("__cycle__");
    }

    let seenIndex = seen.push(node) - 1;
    let keys = Object.keys(node).sort();
    out = "";
    for (i = 0; i < keys.length; i++) {
      let key = keys[i];
      let value = stringify(node[key]);

      if (!value) continue;
      if (out) out += ",";
      out += JSON.stringify(key) + colonSeparator + value;
    }
    seen.splice(seenIndex, 1);
    return "{" + out + "}";
  })(data);
}
