Refactor plan: remove ProcessCmt/ProcessExtra and work directly on CMTs
==============================================================

Goal
----
- Make ResGraph read `.cmt`/`.cmti` directly (no ProcessCmt/ProcessExtra layer) while keeping generation output identical.
- Shape the code so processing can be parallelized (no hidden global state or shared mutation beyond explicit caches passed around).

Current data flow (what happens today)
--------------------------------------
- CLI calls `GenerateSchema.generateSchema`.
- `GenerateSchema` scans project files (via `Packages`) and, for any file that textually contains `@gql.`, loads `Cmt.loadFullCmtFromPath`.
- `Cmt.loadFullCmtFromPath`:
  - Locates package metadata and module name.
  - Calls `ProcessCmt.fileForCmtInfos` to turn `Cmt_format.cmt_infos` into `SharedTypes.File.t`.
    - Builds `Module.structure`/`Module.item` trees representing signature/structure items, with doc/deprecated info from attributes.
    - Creates `Stamps` tables keyed by binding stamps for types/values/modules/constructors.
    - Tracks module visibility via `ModulePath` so exported names can be resolved.
  - Calls `ProcessExtra.getExtra` to build `extra` (reference maps, locItems) used by editor-only features.
  - Returns `full = {file; extra; package}`.
- `GenerateSchema` builds a `QueryEnv` from `full.file` and walks `file.structure` with `traverseStructure`.
  - Decides what to emit based on `Module.item.kind` (Type/Module/Value) and `@gql.*` attributes already attached by ProcessCmt.
  - Field/type extraction uses `SharedTypes.field`, `Constructor.t`, etc. populated by ProcessCmt.
  - Type alias chasing uses `References.digConstructor`.
- `References.digConstructor` and `ResolvePath.resolveFromCompilerPath`:
  - Use `SharedTypes.QueryEnv` + `Stamps` + `Exported` tables built by ProcessCmt.
  - Cross-module lookups call back into `ProcessCmt.fileForModule ~package` to load other modules’ `SharedTypes.File.t` from CMTs.
- `ProcessExtra` is not used by generation, only by reference/hover paths; it still sits in `SharedTypes.full`.
- Global state to note:
  - `SharedTypes.state` holds `packagesByRoot`, `rootForUri`, `cmtCache`.
  - `ProcessCmt.fileForCmt` caches in `SharedTypes.state.cmtCache`.
  - `GenerateSchema` creates its own per-run `cmtCache`, but depends on the ProcessCmt cache underneath.

What generation actually needs from the processed layer
-------------------------------------------------------
- For each module with `@gql.*` inside:
  - Types: record fields (names, optionality, doc/dep, attributes, type_expr), variant constructors (names, payload shapes, doc/dep, attributes), manifests for aliases, rec_status.
  - Values: function type_expr for resolvers (to extract args/return types), doc/dep, attributes.
  - Module nesting and exported names to find the path to a type/value (used by `findTypeLocation`, `extractResolverFunctionInfo`).
  - Ability to resolve a `Types.Path` to the underlying type declaration (alias chasing in `findGraphQLType` / `extractFunctionType` via `References.digConstructor`).
- No generator code currently consumes `extra` (ProcessExtra), completions, hover data, or cursor-aware logic.

Pain points for parallelization
-------------------------------
- Hidden globals (`SharedTypes.state`, ProcessCmt cache, `Cfg.inIncrementalTypecheckingMode`) make it unclear which thread owns what.
- `ProcessCmt.fileForModule` and `ResolvePath` reach into shared caches and mutable Hashtbls; not thread-safe.
- `Exported`/`Stamps` tables are stored globally in `SharedTypes.File.t` and mutated during processing.

Target architecture (CMT-direct)
--------------------------------
- Input: CMT/CMTI files plus package metadata (source roots, module name mapping).
- Pure per-file summary:
  - Read `Cmt_format.cmt_infos` and directly traverse the typed tree to collect only what generation needs: exported types/values/modules with attributes/doc/dep, record fields, variant constructors, aliases/manifests, and module path for locating definitions.
  - Keep summaries immutable; store in a per-run map `module_name -> summary`.
- Path/alias resolution:
  - Build a lightweight resolver that can map `Types.Path` to a `type_declaration` (and module path) using the summaries.
  - Use compiler env (`cmt_initial_env` / cmts’ typed signatures) or a per-run index keyed by `Ident.stamp`/`Path` to avoid global caches.
- Generation:
  - Adapt `GenerateSchema` to consume the new summaries (or an adapter that presents the same shape) and a resolver interface for alias chasing.
  - Keep all caches (CMT summary cache, path resolution cache) passed explicitly through a context so it is trivially parallelizable.
- Eliminate:
  - ProcessCmt/ProcessExtra, `SharedTypes.state.cmtCache`, and any reliance on editor-only extras.

Migration plan (incremental, parity-first)
------------------------------------------
1) Catalog dependencies: list every generator entry point that touches `SharedTypes.Module`/`Stamps`/`ProcessCmt` (done above) and confirm tests cover them.  
2) Define new data model: `ModuleSummary` (types/values/modules + attributes/doc/dep), `TypeDeclSummary` (record/variant/alias/manifests), `ValueSummary` (type_expr + attrs), `ModulePath` info, and a `PathResolver` interface (`resolve_type : Types.Path -> TypeDeclSummary option`).  
3) Implement CMT reader:
   - Walk `Cmt_format.cmt_infos` typedtree/signature to populate `ModuleSummary` directly (no stamps table mutation).
   - Keep it pure and return the summary + any indexes needed for resolution.
4) Implement resolver:
   - Per-run mutable (or purely functional) map of `Path -> TypeDeclSummary`, built from all summaries.
   - Support loading summaries on-demand for external modules (read CMT directly, bypass ProcessCmt caches).
5) Adapter phase:
   - Build an adapter that exposes the minimal parts of `SharedTypes.Module`/`QueryEnv` currently used by `GenerateSchema`, backed by the new summaries, so we can swap the generator with minimal edits.
   - Dual-run mode: run old ProcessCmt pipeline and new CMT-direct pipeline in tests, diff emitted schema to ensure parity.
6) Flip default:
   - Point `GenerateSchema` to the new CMT-direct path + resolver, keep old path behind a flag until stable.
   - Remove ProcessCmt/ProcessExtra/ResolvePath/References dependencies once parity holds.
7) Parallelization-ready cleanup:
   - Thread per-run caches through `GenerateSchema.generateSchema` context.
   - Remove/encapsulate global state (`SharedTypes.state`, ProcessCmt caches); make package resolution injectible/pure where possible.

Open questions / decisions
--------------------------
- How to best resolve `Types.Path` without stamps: prefer using compiler env (Env.find_type) from cmts, but need to confirm we always have enough env to resolve alias chains across modules.  
- Do we preserve `ModulePath`/visibility semantics? If generator only needs exported names, we can simplify, but keep a plan for namespaced modules.  
- How much of `QueryEnv` is actually required once we remove references/hover? Likely only `file`, `exported`, and module path; we can redesign it to be a thin context.
