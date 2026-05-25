import assert from "node:assert/strict";
import { execute, parse } from "graphql";
import { schema } from "./src/__generated__/ResGraphSchema.mjs";

const run = async source => execute({
  schema,
  document: parse(source),
  contextValue: {
    currentUserId: "123",
    dataLoaders: {
      user: {
        load: async () => undefined,
      },
    },
  },
});

const plain = value => JSON.parse(JSON.stringify(value));

const goodTopLevel = await run(`
  query GoodTopLevel {
    goodLabelled {
      __typename
      ... on LabelledBeta {
        count
      }
    }
  }
`);

assert.equal(goodTopLevel.errors, undefined);
assert.deepEqual(plain(goodTopLevel.data), {
  goodLabelled: {
    __typename: "LabelledBeta",
    count: 2,
  },
});

const goodNested = await run(`
  query GoodNested {
    labelledWrapper {
      nested {
        __typename
        ... on LabelledAlpha {
          extra
        }
      }
    }
  }
`);

assert.equal(goodNested.errors, undefined);
assert.deepEqual(plain(goodNested.data), {
  labelledWrapper: {
    nested: {
      __typename: "LabelledAlpha",
      extra: "nested alpha",
    },
  },
});

const badTopLevel = await run(`
  query BadTopLevel {
    badLabelled {
      __typename
      ... on LabelledAlpha {
        extra
      }
      ... on LabelledBeta {
        count
      }
    }
  }
`);

assert.ok(Array.isArray(badTopLevel.errors));
assert.match(
  badTopLevel.errors[0].message,
  /Interface Labelled resolveType expected a tagged value from Interface_labelled\.Resolver\.t/,
);

const badNested = await run(`
  query BadNested {
    brokenLabelledWrapper {
      nested {
        __typename
        ... on LabelledAlpha {
          extra
        }
        ... on LabelledBeta {
          count
        }
      }
    }
  }
`);

assert.ok(Array.isArray(badNested.errors));
assert.match(
  badNested.errors[0].message,
  /Interface Labelled resolveType expected a tagged value from Interface_labelled\.Resolver\.t/,
);

const reservedWords = await run(`
  query ReservedWords {
    reservedWordRecord {
      constraint
      external
      include
      let
      module
      open
      switch
      type
    }
  }
`);

assert.equal(reservedWords.errors, undefined);
assert.deepEqual(plain(reservedWords.data), {
  reservedWordRecord: {
    constraint: "constraint",
    external: "external",
    include: "include",
    let: "let",
    module: "module",
    open: "open",
    switch: "switch",
    type: "type",
  },
});

const reservedInput = await run(`
  query ReservedInput {
    reservedWordInputEcho(input: {constraint: "input constraint", type: "input type"})
  }
`);

assert.equal(reservedInput.errors, undefined);
assert.deepEqual(plain(reservedInput.data), {
  reservedWordInputEcho: "input constraint:input type",
});

const reservedArgument = await run(`
  query ReservedArgument {
    reservedWordArgumentEcho(constraint: "arg constraint")
  }
`);

assert.equal(reservedArgument.errors, undefined);
assert.deepEqual(plain(reservedArgument.data), {
  reservedWordArgumentEcho: "arg constraint",
});

const explicitNamed = await run(`
  query ExplicitNamed {
    explicitNamed {
      __typename
      ... on ExplicitCompany {
        name
        entityKind
      }
    }
  }
`);

assert.equal(explicitNamed.errors, undefined);
assert.deepEqual(plain(explicitNamed.data), {
  explicitNamed: {
    __typename: "ExplicitCompany",
    name: "Informind",
    entityKind: "Company",
  },
});

const explicitSearchable = await run(`
  query ExplicitSearchable {
    explicitSearchable {
      __typename
      ... on ExplicitSearchResult {
        id
        label(prefix: "result")
      }
    }
  }
`);

assert.equal(explicitSearchable.errors, undefined);
assert.deepEqual(plain(explicitSearchable.data), {
  explicitSearchable: {
    __typename: "ExplicitSearchResult",
    id: "search-result",
    label: "result:searchable",
  },
});

const explicitContextResult = await run(`
  query ExplicitContextResult {
    explicitContextResult {
      id
      contextLabel
    }
  }
`);

assert.equal(explicitContextResult.errors, undefined);
assert.deepEqual(plain(explicitContextResult.data), {
  explicitContextResult: {
    id: "ctx-result",
    contextLabel: "override-no-ctx",
  },
});

const explicitContextOverrideResult = await run(`
  query ExplicitContextOverrideResult {
    explicitContextOverrideResult {
      id
      contextOverrideLabel
    }
  }
`);

assert.equal(explicitContextOverrideResult.errors, undefined);
assert.deepEqual(plain(explicitContextOverrideResult.data), {
  explicitContextOverrideResult: {
    id: "ctx-override-result",
    contextOverrideLabel: "override:123",
  },
});

console.log("runtime regressions passed");
