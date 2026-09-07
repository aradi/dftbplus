# AGENTS.md — DFTB+ developer guide for coding agents

This file collects the project-specific knowledge an agent needs to make safe,
idiomatic changes to DFTB+. It complements (does not replace)
`CONTRIBUTING.rst`, `INSTALL.rst`, and the official developer's guide at
<http://dftbplus-develguide.readthedocs.io/en/latest/>. When in doubt, prefer
what you observe in neighbouring source files over what is written here — this
document can go stale, the code cannot.

## What DFTB+ is

DFTB+ is a Fortran quantum-chemistry/atomistic-simulation package (Density
Functional Tight Binding). It is used both as a standalone application and as
a library embedded in other software (via a C/Python-accessible API). It is
distributed under the LGPL and used by a large external research community, so
**numerical correctness, reproducibility, and backward compatibility of input
files/API are taken very seriously**. A silent numerical regression is worse
than a crash: prefer failing loudly over producing a plausible-looking wrong
answer.

## Repository layout

```
src/dftbp/          Main Fortran library, organised by physics/functionality:
  common/            precision, environment, MPI/BLACS env, status/error types, I/O globals
  type/              Core derived types (orbitals, commontypes, etc.)
  dftb/              Core DFTB Hamiltonian/energy/charge machinery
  dftbplus/          Top-level driver/orchestration logic for the main calculation
  elecsolvers/       Eigensolvers / electronic structure solvers
  geometry/, geoopt/ Geometry handling and optimisation
  io/                Input/output, HSD parsing, tagged output
  math/              Numerical utilities (linear algebra helpers, quadrature, ...)
  md/                Molecular dynamics drivers/thermostats
  mixer/             SCC charge mixers
  solvation/         Implicit solvation models
  timedep/           Time-dependent DFTB (excited states)
  transport/         Transport / NEGF-related code (build-gated by WITH_TRANSPORT)
  poisson/           Poisson solver (build-gated by WITH_POISSON)
  reks/              REKS multi-reference method
  xtb/               xTB related code
  extlibs/           Thin Fortran wrappers around external libraries
  api/               Public C/Fortran API exposed when WITH_API is set
  include/           fypp preprocessor macro files (*.fypp), shared across the codebase
app/                 Executables (dftb+, waveplot, modes, phonons, transporttools, misc tools)
test/
  src/dftbp/unit/    Fortuno-based unit tests, mirrors src/dftbp/ layout
  src/dftbp/integration/, api/  Integration/API-level tests
  app/dftb+/<category>/<case>/  Regression tests: dftb_in.hsd + expected *.tag output
doc/dftb+/           Manual (LaTeX), FORD/Doxygen API doc config
external/            Git submodules for optional/bundled dependencies (DO NOT edit in place)
cmake/, sys/         CMake helper modules and per-compiler toolchain files
config.cmake         Central list of WITH_* build options and their defaults
utils/               Helper scripts (get_opt_externals, test drivers, release tooling)
```

Module → file → path correspondence is strict: a module named
`dftbp_dftb_charges` lives in `src/dftbp/dftb/charges.F90`. When looking for a
symbol, derive the file path from the module name (`dftbp_<dir>_<name>` →
`src/dftbp/<dir>/<name>.F90`) rather than grepping blindly.

## Build system

- CMake drives the build; `.F90`/`.fypp` files are preprocessed with
  **fypp** before compilation. Non-preprocessed Fortran uses lowercase `.f90`.

- Optional components are toggled through `WITH_*` CMake options defined in
  `config.cmake` (MPI, OpenMP, GPU, ELSI/ELPA, transport/Poisson, various
  dispersion/xTB backends, API, unit tests, ...). Code that depends on an
  optional component must be guarded with the corresponding fypp `WITH_*`
  variable (see `src/dftbp/include/common.fypp`) so the build still succeeds
  with the component disabled.

- Many external dependencies are git submodules under `external/` (mpifx,
  scalapackfx, libnegf, mbd, tblite, s-dftd3, dftd4, toml-f, chimes, fortuno,
  ...) fetched via `./utils/get_opt_externals`. Never hand-edit files inside
  `external/`; if a fix is needed there, it belongs upstream.

- Typical configure/build/test cycle:

  ```
  FC=gfortran CC=gcc cmake -DCMAKE_INSTALL_PREFIX=$PWD/_install -GNinja -B _build .
  cmake --build _build
  pushd _build && ctest -j && popd
  ```
  Re-run CMake (or delete `_build/CMakeCache.txt`) after changing
  `config.cmake` or toolchain files in `sys/`. A conda/pixi environment
  (`pixi.toml`) is also available and pins compiler/tool versions used in CI.

- New source files must be registered explicitly — CMake does **not** glob.
  Add the new `.F90`/`.fypp` filename to the `sources-fpp`/`sources-f90` list
  in the relevant `src/dftbp/<dir>/CMakeLists.txt` (and analogously
  `sources-fypp` in `test/src/dftbp/unit/<dir>/CMakeLists.txt` for a new unit
  test file). Forgetting this is the most common reason a new file "does
  nothing".

## Fortran coding conventions

These are inferred from the existing codebase; match the style of the file
you are editing over anything listed here.

### File structure and formatting

- Every source file starts with the standard license header block (copy it
  verbatim from a neighbouring file — including the DFTB+ developers group
  copyright line — do not invent a new one).

- Indentation is **2 spaces**, no tabs. Continuation lines are aligned to
  their surrounding block and indented **4 spaces**.

- Line length is capped at **100 characters** (the license header's divider
  line is exactly 100 chars — a good visual ruler).

- One module per file; module name is `dftbp_<subdirectory>_<filename>`
  (e.g. `src/dftbp/dftb/charges.F90` → `dftbp_dftb_charges`).

- `implicit none` is mandatory in every module.

- Modules are `private` by default, with an explicit `public ::` list naming
  everything the module exports. Do not make things public "just in case".

- fypp preprocessed files (`.F90`) start with `#:include 'common.fypp'` (and
  `error.fypp` if error macros are used) right after the license header.

### Documentation comments (FORD-style)

- Every public module, type, procedure, and dummy argument gets a doc
  comment: `!>` for the first line, `!!` to continue a paragraph, placed
  **directly above** the item it documents.

- Document each dummy argument of public routines individually, right above its
  declaration — do not bundle argument docs into one blob at the top of the
  procedure. State units, expected array shape/ordering (e.g. `Shape [mOrb,
  nAtom]`), and any non-obvious invariant (tolerances, sign conventions, whether
  the array is overwritten).

- Do not restate the obvious; do explain physical meaning, units, and
  conventions that aren't derivable from the name/type alone.

### Naming

- Derived types are named `T<PascalCase>` (e.g. `TStatus`, `TOrbitals`,
  `TUniqueHubbard`).

- Procedures and variables use `camelCase` (`getSummedCharges`, `nAtom`,
  `dQAtom`). Module-level parameters also use camelCase (`elecTolMax`,
  `minNeighDist`).

- Boolean flags read as questions/predicates where possible
  (`hasError`, `isOk`). Older style prefixed with `t` (e.g. `tPeriodic`) appear
  too, use this only if that is the convention in the module you're editing,
  in all other case use the question/predicates convention.

- Procedures should be verbs (`calculateDensity`, `updateCoordinates`), type
  names and variables (apart from Booleans) should be nouns (the former
  prefixed by a capital `T`).


### Types, kinds, and precision

- Never use bare `real`/`double precision` literals or kinds. Use the
  parameter `dp` from `dftbp_common_accuracy` and suffix real literals with
   `_dp` (`1.0_dp`, not `1.0d0` or `1.0`).

- There are some string lenght constants as well (`sc`, `mc`, `lc`). When
  adding new code, use allocated character variables instead, whenever possible.

- Reuse existing tolerance/threshold constants from `dftbp_common_accuracy`
  (`elecTolMax`, `tolSameDist`, `minTemp`, ...) instead of hard-coding new
  magic numbers; add a new named constant there if a genuinely new tolerance
  is needed, with a doc comment explaining its purpose.

### Procedure interfaces

- Every dummy argument has an explicit `intent` (`in`, `out`, or `inout`) —
  never omit it.

- Mark procedures `pure`/`elemental` whenever their body allows it (see
  `getSummedChargesPerOrbital` for an example); this both documents intent and
  lets the compiler catch accidental side effects.

- Prefer assumed-shape arrays (`real(dp), intent(in) :: x(:,:)`) over
  explicit-shape/assumed-size dummy arguments in new code.

- `use` statements are always scoped with `only :` — bare `use module_name`
  (importing everything) is essentially never used in this codebase; keep it
  that way so dependency graphs stay legible and grep-able.

- Optional arguments are used deliberately to provide alternate outputs/modes
  from a single routine (see `getSummedCharges`), guarded with
  `present(...)` checks — not as a substitute for splitting a routine that
  does two unrelated things.

### Defensive programming

General defensive-programming expectations:

- Validate array shapes/sizes and physically meaningful ranges (occupation
  numbers, charges, temperatures, indices into species/atom lists) at
  routine boundaries that receive external/user data; use `@:ASSERT` for
  internal invariants deeper in the call stack.

- Never silently truncate, clamp, or "fix up" invalid numerical input — raise
  an error with a clear message so the user can correct their input.

- Avoid uninitialised variables; give derived-type components sensible
  defaults (see `TStatus`'s `code = 0`) so a forgotten initialisation fails
  safe.

- Be careful with floating-point comparisons — always compare against the
  tolerance constants.

- Watch allocation lifettimes: prefer `allocatable` over raw `pointer` unless
  aliasing is genuinely required (as in `charges.F90`'s `dQWork` pattern,
  where a pointer lets an optional output argument double as working
  storage); always pair manual `pointer`/`target` usage with a clear
  ownership story in the doc comment. Reach for `pointer` only for that kind
  of genuine aliasing, never as a substitute for `allocatable`.

### Style/features to avoid

- No `goto`, no `common` blocks, no fixed-form source.

- No implicit typing (`implicit none` is non-negotiable).

- Avoid bare `use module` without `only`.

- Avoid adding new `error stop`/`stop` calls deep in library code — reserve
  those for `dftbp_common_globalenv`-style top-level abort paths and genuine
  application entry points.

- Don't hand-roll physical/mathematical constants that already exist in
  `dftbp_common_accuracy` or the relevant unit-conversion modules
  (`dftbp_common_unitconversion`).

- Prefer modern Fortran constructs already used in the codebase (`block`
  constructs, `move_alloc`, assumed-shape arrays, `associate`, structure
  constructors) over older idioms.

## Preprocessor (fypp) notes

- `#:include 'common.fypp'` brings in the `WITH_*` feature flags, the
  `@:ASSERT`/`@:DEBUG_CODE`/`@:REQUIRES_COMPONENT` macros, and misc helpers
  (`FORTRAN_LOGICAL`, `CREATE_CLASS`, ...).

- `#:include 'error.fypp'` brings in `TStatus`-related error macros.

- Guard code that depends on an optional external component with
  `#:if WITH_<COMPONENT>` (compile-time) or `@:REQUIRES_COMPONENT(name,
  available, code)` when the availability is only known at a call site within
  always-compiled code.

- Macro/template files live in `src/dftbp/include/*.fypp`
  (`allocatablelist.fypp`, `pointerlist.fypp`, etc.) — reuse them for
  generic-container needs instead of writing a bespoke implementation.

## Testing

Two independent, complementary test layers — a code change should usually
touch both when it changes behaviour:

1. **Unit tests** (`test/src/dftbp/unit/`, built with `-DWITH_UNIT_TESTS=true`,
   using the **Fortuno** framework). Layout mirrors `src/dftbp/`. A new test
   file needs both `#:include "fortuno_serial.fypp"` boilerplate (see
   `test/src/dftbp/unit/common/atomicmass.F90` for the canonical shape) and an
   entry in the corresponding `test/src/dftbp/unit/<dir>/CMakeLists.txt`
   `sources-fypp` list. Use `@:ASSERT(...)` inside `$:TEST("name") ... $:END_TEST()`
   blocks for the actual checks.

2. **Application regression tests** (`test/app/dftb+/<category>/<case>/`,
   run via `ctest`): each case is a directory with a `dftb_in.hsd` input and
   an expected `_autotest.tag`/output; the harness runs the built `dftb+`
   binary and diffs tagged output against the reference within numerical
   tolerance. Adding a new physical feature/code path typically means adding
   a new case directory (copy an existing similar case as a template) rather
   than editing the driver.

- Build and run everything with:
  ```
  cmake -B _build -DWITH_UNIT_TESTS=true -GNinja .
  cmake --build _build
  pushd _build && ctest -j && popd
  ```
  Set `-DTEST_MPI_PROCS=` / `-DTEST_OMP_THREADS=` to match available cores
  when testing MPI/OpenMP builds; reduce OpenMP threads accordingly when
  running tests in parallel with `ctest -j`.

- Run a single test/category with `ctest -R <regex>` (e.g.
  `ctest -R unit/common` or a substring of a regression test's directory
  name) instead of the full suite while iterating.

- Existing regression/unit test directories are strong references for
  expected `.hsd` input syntax and tagged output format — imitate the
  nearest existing case rather than inventing new conventions.

## Non-Fortran parts

- `src/dftbp/api/` plus the C/Python bindings under `test/src/dftbp/api/`
  form the public API surface (`WITH_API`); API/ABI changes are
  compatibility-sensitive — bump/document the API version
  (`DFTBPLUS_VERSION`/`API_VERSION` machinery in `CMakeLists.txt`) and update
  `doc/api/` docs when the public interface changes.

- `dptools` (Python utilities, `doc/dptools/`) and `test/app/dftb+/bin/` test
  helpers (e.g. `test_tagreader.py`) are plain Python — standard PEP 8
  expectations apply there; no special project convention beyond that.

## Documentation & metadata to keep in sync

- `CHANGELOG.rst` — add a bullet under the `Unreleased` → `Added`/`Changed`/
  `Fixed` section for any user-visible change (new keyword, new feature,
  bug fix, behaviour change). Follow the existing terse, imperative style.

- `doc/dftb+/manual/*.tex` — update when adding/changing HSD input keywords
  or documented behaviour; the manual is the authoritative user-facing
  reference, and undocumented keywords are effectively unusable by end users.

- FORD (`doc/dftb+/ford/`) and Doxygen (`doc/dftb+/doxygen/`) API docs are
  generated from the `!>`/`!!` comments in the source — there is nothing
  separate to maintain by hand as long as doc comments are kept current.

- `AUTHORS.rst` — contributors add themselves (alphabetical by family name);
  don't add this on a contributor's behalf without being asked.

## Git / contribution workflow

- Default branch is `main`; PRs are made against it (see `CONTRIBUTING.rst`).

- Every contribution must comply with the Developer Certificate of Origin
  (real name + email in commits — no pseudonymous/anonymous commits).

- This repo uses git submodules (`external/*`) — do not commit changes inside
  a submodule from the superproject, and be careful with commands that touch
  submodule state.

- Follow the git safety practices already required of you as an agent (no
  force-push/reset --hard/history rewrite without explicit confirmation,
  never bypass hooks); this project's CI and regression-test coverage is the
  primary safety net for numerical correctness, so don't skip or weaken tests
  to make a build pass.
