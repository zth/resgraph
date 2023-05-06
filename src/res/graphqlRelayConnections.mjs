/**
Copyright (c) GraphQL Contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. */

// Most of this file is inlined from https://github.com/graphql/graphql-relay-js

/* Mostly taken from `url-safe-base64` */

const ENC = {
  "+": "-",
  "/": "_",
  "=": ",",
};
const DEC = {
  "-": "+",
  _: "/",
  ",": "=",
};

const encode = (base64) => {
  return base64.replace(/[+/=]/g, (m) => ENC[m]);
};

const decode = (safe) => {
  return safe.replace(/[-_.,]/g, (m) => DEC[m]);
};

export const encodeBase64 = (content) =>
  encode(Buffer.from(content).toString("base64"));

export const decodeBase64 = (content) =>
  Buffer.from(decode(content), "base64").toString("utf-8");

/**
 * A simple function that accepts an array and connection arguments, and returns
 * a connection object for use in GraphQL. It uses array offsets as pagination,
 * so pagination will only work if the array is static.
 */
export function connectionFromArray(data, args) {
  return connectionFromArraySlice(data, args, {
    sliceStart: 0,
    arrayLength: data.length,
  });
}

/**
 * A version of `connectionFromArray` that takes a promised array, and returns a
 * promised connection.
 */
export function connectionFromPromisedArray(dataPromise, args) {
  return dataPromise.then((data) => connectionFromArray(data, args));
}

/**
 * Given a slice (subset) of an array, returns a connection object for use in
 * GraphQL.
 *
 * This function is similar to `connectionFromArray`, but is intended for use
 * cases where you know the cardinality of the connection, consider it too large
 * to materialize the entire array, and instead wish pass in a slice of the
 * total result large enough to cover the range specified in `args`.
 */
export function connectionFromArraySlice(arraySlice, args, meta) {
  const { after, before, first, last } = args;
  const { sliceStart, arrayLength } = meta;
  const sliceEnd = sliceStart + arraySlice.length;

  let startOffset = Math.max(sliceStart, 0);
  let endOffset = Math.min(sliceEnd, arrayLength);

  const afterOffset = getOffsetWithDefault(after, -1);
  if (0 <= afterOffset && afterOffset < arrayLength) {
    startOffset = Math.max(startOffset, afterOffset + 1);
  }

  const beforeOffset = getOffsetWithDefault(before, endOffset);
  if (0 <= beforeOffset && beforeOffset < arrayLength) {
    endOffset = Math.min(endOffset, beforeOffset);
  }

  if (typeof first === "number") {
    if (first < 0) {
      throw new Error('Argument "first" must be a non-negative integer');
    }

    endOffset = Math.min(endOffset, startOffset + first);
  }
  if (typeof last === "number") {
    if (last < 0) {
      throw new Error('Argument "last" must be a non-negative integer');
    }

    startOffset = Math.max(startOffset, endOffset - last);
  }

  // If supplied slice is too large, trim it down before mapping over it.
  const slice = arraySlice.slice(
    startOffset - sliceStart,
    endOffset - sliceStart
  );

  const edges = slice.map((value, index) => ({
    cursor: offsetToCursor(startOffset + index),
    node: value,
  }));

  const firstEdge = edges[0];
  const lastEdge = edges[edges.length - 1];
  const lowerBound = after != null ? afterOffset + 1 : 0;
  const upperBound = before != null ? beforeOffset : arrayLength;
  return {
    edges,
    pageInfo: {
      startCursor: firstEdge ? firstEdge.cursor : null,
      endCursor: lastEdge ? lastEdge.cursor : null,
      hasPreviousPage:
        typeof last === "number" ? startOffset > lowerBound : false,
      hasNextPage: typeof first === "number" ? endOffset < upperBound : false,
    },
  };
}

/**
 * A version of `connectionFromArraySlice` that takes a promised array slice,
 * and returns a promised connection.
 */
export function connectionFromPromisedArraySlice(dataPromise, args, arrayInfo) {
  return dataPromise.then((data) =>
    connectionFromArraySlice(data, args, arrayInfo)
  );
}

const PREFIX = "arrayconnection:";

/**
 * Creates the cursor string from an offset.
 */
export function offsetToCursor(offset) {
  return encodeBase64(PREFIX + offset.toString());
}

/**
 * Extracts the offset from the cursor string.
 */
export function cursorToOffset(cursor) {
  return parseInt(decodeBase64(cursor).substring(PREFIX.length), 10);
}

/**
 * Return the cursor associated with an object in an array.
 */
export function cursorForObjectInConnection(data, object) {
  const offset = data.indexOf(object);
  if (offset === -1) {
    return null;
  }
  return offsetToCursor(offset);
}

/**
 * Given an optional cursor and a default offset, returns the offset
 * to use; if the cursor contains a valid offset, that will be used,
 * otherwise it will be the default.
 */
export function getOffsetWithDefault(cursor, defaultOffset) {
  if (typeof cursor !== "string") {
    return defaultOffset;
  }
  const offset = cursorToOffset(cursor);
  return isNaN(offset) ? defaultOffset : offset;
}
