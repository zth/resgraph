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

// Entire file is inlined from https://github.com/graphql/graphql-relay-js

export function base64(input) {
  const utf8Array = stringToUTF8Array(input);
  let result = "";

  const length = utf8Array.length;
  const rest = length % 3;
  for (let i = 0; i < length - rest; i += 3) {
    const a = utf8Array[i];
    const b = utf8Array[i + 1];
    const c = utf8Array[i + 2];

    result += first6Bits(a);
    result += last2BitsAndFirst4Bits(a, b);
    result += last4BitsAndFirst2Bits(b, c);
    result += last6Bits(c);
  }

  if (rest === 1) {
    const a = utf8Array[length - 1];
    result += first6Bits(a) + last2BitsAndFirst4Bits(a, 0) + "==";
  } else if (rest === 2) {
    const a = utf8Array[length - 2];
    const b = utf8Array[length - 1];
    result +=
      first6Bits(a) +
      last2BitsAndFirst4Bits(a, b) +
      last4BitsAndFirst2Bits(b, 0) +
      "=";
  }

  return result;
}

function first6Bits(a) {
  return toBase64Char((a >> 2) & 63);
}

function last2BitsAndFirst4Bits(a, b) {
  return toBase64Char(((a << 4) | (b >> 4)) & 63);
}

function last4BitsAndFirst2Bits(b, c) {
  return toBase64Char(((b << 2) | (c >> 6)) & 63);
}

function last6Bits(c) {
  return toBase64Char(c & 63);
}

export function unbase64(input) {
  const utf8Array = [];

  for (let i = 0; i < input.length; i += 4) {
    const a = fromBase64Char(input[i]);
    const b = fromBase64Char(input[i + 1]);
    const c = fromBase64Char(input[i + 2]);
    const d = fromBase64Char(input[i + 3]);

    if (a === -1 || b === -1 || c === -1 || d === -1) {
      /*
       * Previously we used Node's API for parsing Base64 and following code
       * Buffer.from(i, 'utf8').toString('base64')
       * That silently ignored incorrect input and returned empty string instead
       * Let's keep this behaviour for a time being and hopefully fix it in the future.
       */
      return "";
    }

    const bitmap24 = (a << 18) | (b << 12) | (c << 6) | d;
    utf8Array.push((bitmap24 >> 16) & 255);
    utf8Array.push((bitmap24 >> 8) & 255);
    utf8Array.push(bitmap24 & 255);
  }

  let paddingIndex = input.length - 1;
  while (input[paddingIndex] === "=") {
    --paddingIndex;
    utf8Array.pop();
  }

  return utf8ArrayToString(utf8Array);
}

const b64CharacterSet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

function toBase64Char(bitMap6) {
  return b64CharacterSet.charAt(bitMap6);
}

function fromBase64Char(base64Char) {
  if (base64Char === undefined) {
    return -1;
  }
  return base64Char === "=" ? 0 : b64CharacterSet.indexOf(base64Char);
}

function stringToUTF8Array(input) {
  const result = [];
  for (const utfChar of input) {
    const code = utfChar.codePointAt(0);
    if (code < 0x80) {
      result.push(code);
    } else if (code < 0x800) {
      result.push(0xc0 | (code >> 6));
      result.push(0x80 | (code & 0x3f));
    } else if (code < 0x10000) {
      result.push(0xe0 | (code >> 12));
      result.push(0x80 | ((code >> 6) & 0x3f));
      result.push(0x80 | (code & 0x3f));
    } else {
      result.push(0xf0 | (code >> 18));
      result.push(0x80 | ((code >> 12) & 0x3f));
      result.push(0x80 | ((code >> 6) & 0x3f));
      result.push(0x80 | (code & 0x3f));
    }
  }
  return result;
}

function utf8ArrayToString(input) {
  let result = "";
  for (let i = 0; i < input.length; ) {
    const a = input[i++];
    if ((a & 0x80) === 0) {
      result += fromCodePoint(a);
      continue;
    }

    const b = input[i++];
    if ((a & 0xe0) === 0xc0) {
      result += fromCodePoint(((a & 0x1f) << 6) | (b & 0x3f));
      continue;
    }

    const c = input[i++];
    if ((a & 0xf0) === 0xe0) {
      result += fromCodePoint(
        ((a & 0x0f) << 12) | ((b & 0x3f) << 6) | (c & 0x3f)
      );
      continue;
    }

    const d = input[i++];
    result += fromCodePoint(
      ((a & 0x07) << 18) | ((b & 0x3f) << 12) | ((c & 0x3f) << 6) | (d & 0x3f)
    );
  }

  return result;
}

function fromCodePoint(code) {
  if (code > 0x10ffff) {
    /*
     * Previously we used Node's API for parsing Base64 and following code
     * Buffer.from(i, 'base64').toString('utf8')
     * That silently ignored incorrect input and returned empty string instead
     * Let's keep this behaviour for a time being and hopefully fix it in the future.
     */
    return "";
  }
  return String.fromCodePoint(code);
}

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
  return base64(PREFIX + offset.toString());
}

/**
 * Extracts the offset from the cursor string.
 */
export function cursorToOffset(cursor) {
  return parseInt(unbase64(cursor).substring(PREFIX.length), 10);
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
