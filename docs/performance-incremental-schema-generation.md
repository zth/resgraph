# Incremental schema generation and direct formatting

Date: 2026-07-29

## Summary

Schema generation was dominated by two avoidable costs:

- Building large output strings with repeated concatenation.
- Parsing the generated ReScript back into an AST and running the general-purpose ReScript pretty-printer over it.

ResGraph now emits readable, deterministic ReScript directly through a small `CodeWriter` backed by `Buffer`. It no longer invokes the ReScript parser/pretty-printer for generated schema or interface files.

A conservative incremental cache skips generation when every known input and generated output is unchanged. It deliberately prefers false misses over false hits: any changed source, CMT, source directory, configuration file, lockfile, executable, setting, or output causes a full regeneration.

## Direct writer

`CodeWriter` provides only four layout concepts:

- Append text.
- Append a line or blank line.
- Run a callback at one additional indentation level.
- Return the accumulated buffer.

It does not measure line widths, search alternative layouts, or parse generated syntax. Schema-specific printers decide where records, field maps, enum values, and schema type lists should break. Opaque `%raw` JavaScript remains untouched.

This output is intentionally "best effort" rather than byte-identical to `rescript format`. It is structured enough to inspect and debug while remaining linear in output size.

## Conservative incremental cache

The cache is stored in `lib/.resgraphIncrementalCache`, alongside other ignored build artifacts. It records:

- Source and output folders, generation flags, and debug mode.
- The ResGraph executable signature.
- Project configuration, package manifests, and lockfiles.
- Every source and CMT/CMTI discovered for project and dependency modules.
- Immediate module directories and all configured source directories, detecting additions and removals.
- Generated schema, interface, SDL, and state outputs with content digests.

A hit requires every input signature to match. Generated outputs use signatures first and content digests when metadata changed, allowing byte-identical output rewrites without a full build. Missing, incompatible, or corrupt cache data is ignored and rebuilt. Cache writes use a temporary file followed by rename and start with a textual version header before the marshalled payload.

Compiling newly generated output can rewrite compiled inputs. The next generation may therefore conservatively refresh once before reaching a stable hit.

Environment controls:

- `RESGRAPH_INCREMENTAL_CACHE=false` disables cache reads and writes.
- `RESGRAPH_INCREMENTAL_DEBUG=1` prints hit and miss reasons to stderr.

## HockeySwedes benchmark

Benchmark project: HockeySwedes `7f9a356`, containing 1,360 project ReScript sources and 1,503 compiled modules. Both binaries used OCaml 5.3.0 and Dune's release profile. Each sample started a fresh native process with warm filesystem caches. Ten alternating samples were collected per binary on a shared host.

| Metric | `main` | Direct writer | Change |
| --- | ---: | ---: | ---: |
| Median full generation | 850 ms | 505 ms | -40.6% |
| Mean full generation | 795 ms | 484 ms | -39.1% |
| Mean CPU time | 671 ms | 369 ms | -45.0% |
| Mean maximum RSS | 103.2 MB | 53.0 MB | -48.6% |

Generated output changed from 891,468 bytes across 24,892 formatter-produced lines to 804,624 bytes across 16,924 directly formatted lines. HockeySwedes compiled successfully with the direct output.

After the conservative cache converged, 100 fresh-process hits took 2.14 seconds in aggregate, or about 21.4 ms each. Maximum RSS was 12.8 MB. A syscall profile recorded 3,069 `newfstatat` calls and about 8.9 ms total syscall time; validation is intentionally broad because this first version favors simple correctness over GraphQL-aware invalidation.

## Correctness behavior

- Stable inputs and outputs skip generation.
- Any project or dependency source/CMT metadata change regenerates.
- Adding or removing source files/directories regenerates.
- Configuration, lockfile, executable, or generation-setting changes regenerate.
- Generated output edits regenerate and restore deterministic output.
- Invalid or incompatible cache data regenerates without unmarshalling an unknown layout.
- `writeIfHasChanges` still avoids rewriting byte-identical generated files.

## Potential follow-up

Measure cache misses in normal watch workflows before making invalidation more selective. If unrelated ReScript edits cause material rebuild overhead, possible refinements include tracking the exact CMT dependency closure or rescanning only changed sources for `@gql.`. Those optimizations should be added only with targeted invalidation tests; the conservative cache is easier to reason about and already makes stable no-op generation inexpensive.
