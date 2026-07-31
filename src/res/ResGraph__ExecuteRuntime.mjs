import {execute, validate} from "graphql";

const validatedDocumentsBySchema = new WeakMap();
export function createQueryDocumentCache(maxSize = 100) {
  if (!Number.isSafeInteger(maxSize) || maxSize < 1) {
    throw new RangeError("ResGraph query cache maxSize must be a positive integer.");
  }
  return {maxSize, documents: new Map()};
}

export function getCachedQuery(cache, query) {
  const document = cache.documents.get(query);
  if (document !== undefined) {
    cache.documents.delete(query);
    cache.documents.set(query, document);
  }
  return document;
}

export function setCachedQuery(cache, query, document) {
  if (cache.documents.has(query)) {
    cache.documents.delete(query);
  } else if (cache.documents.size >= cache.maxSize) {
    const oldestQuery = cache.documents.keys().next().value;
    cache.documents.delete(oldestQuery);
  }
  cache.documents.set(query, document);
}


function validationErrors(schema, document) {
  let validatedDocuments = validatedDocumentsBySchema.get(schema);
  if (validatedDocuments?.has(document)) {
    return [];
  }

  const errors = validate(schema, document);
  if (errors.length === 0) {
    if (validatedDocuments === undefined) {
      validatedDocuments = new WeakSet();
      validatedDocumentsBySchema.set(schema, validatedDocuments);
    }
    validatedDocuments.add(document);
  }
  return errors;
}

export function executeValidated(args) {
  return Promise.resolve().then(() => {
    const errors = validationErrors(args.schema, args.document);
    return errors.length > 0 ? {errors} : execute(args);
  });
}
