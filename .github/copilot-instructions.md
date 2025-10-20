# Copilot Instructions for R-SVN Codebase

## Project Overview
- This is a mirror of the R language [svn-trunk](https://svn.r-project.org/R/trunk/), used for tracking changes and testing patches via GitHub Actions.
- The codebase contains the full source tree for R, including C, Fortran, and R code, build scripts, and documentation.
- The main goal for this codebase is to refactor R from C to C++ while maintaining compatibility with upstream R, enabling patch testing, and supporting CI builds across platforms.
- **C++ Refactoring Strategy:**
  - Prioritize refactoring core interpreter and modules in `src/` from C to C++.
  - Maintain API/ABI compatibility with upstream R; avoid breaking changes unless coordinated.
  - Use modern C++ features where possible, but preserve legacy interfaces for compatibility.
  - Document refactoring decisions in code comments and `doc/notes/`.
  - Run full test suite (`make check`) after each major refactor.

## Architecture & Key Directories
- `src/`: Core source code (C, C++, Fortran, R). Subdirectories for main interpreter, modules, math, platform-specific code.
- `library/`: Base and recommended R packages.
- `bin/`: Shell wrappers for R, build, check, install, and other developer tools. These scripts orchestrate builds, package management, and developer workflows.
- `doc/`: Documentation, manuals, notes, and changelogs.
- `etc/`: Configuration files, environment settings, and Makefiles.
- `Makefile`, `Makeconf`: Top-level build orchestration.

## Developer Workflows
- **Build from source:**
  ```sh
  ./configure CC=gcc CXX=g++ --enable-R-shlib --with-blas --with-lapack --enable-werror
  make
  make check
  ```
  - See `INSTALL` and `doc/html/R-admin.html` for details.
- **Run R:**
  - Use `bin/R` for launching R from the source tree.
- **Package management:**
  - Use `bin/INSTALL`, `bin/REMOVE`, `bin/build`, `bin/check` for add-on packages. These wrap R commands and set up environment variables.
- **Manuals:**
  - Build with `make pdf` or `make info`.
- **Tagging source files:**
  - Use `bin/rtags` for generating Emacs/Vi tags for C, R, and Rd files.

## Conventions & Patterns
- Original r-svn project is referred to as 'CR', while the new, refactored version is called 'CXXR'.
- CR's API remains wrapped in extern "C". Previously extern "C" functions and variables that are not part of CR's API were moved to "R" namespace.
- New functions and variables should be placed in "CXXR" namespace.
- **Scripts in `bin/`** use environment variables like `R_HOME`, `R_SHARE_DIR`, and `R_INCLUDE_DIR` for portability.
- **Arguments to scripts** are often passed as `nextArg<value>` for later parsing in R code.
- **Platform-specific logic** is handled in shell scripts and Makefiles (see `bin/R`, `Makefile`).
- **Documentation** is maintained in `doc/NEWS.Rd`, `doc/FAQ`, and other files. Changelogs are in `ChangeLog` and `doc/NEWS*`.
- **Pain points & gotchas:**
  - Some build flags and platform logic are non-standard; always check `Makefile`, `bin/R`, and `INSTALL` for details.
  - Refactoring may expose subtle memory management or API issues—run `make check` and inspect logs.
  - Windows builds may require extra configuration (see CI logs and `doc/html/R-admin.html`).
  - When in doubt, consult `doc/notes/` for implementation details and historical context.

## Integration Points
- **External dependencies:**
  - BLAS/LAPACK, Java, Tcl/Tk, and compilers are configured via `configure` and `bin/config`.
- **CI/CD:**
  - GitHub Actions workflows in `.github/workflows/` automate builds and tests.

## Examples
- To build and check R:
  ```sh
  ./configure CC=gcc CXX=g++ --enable-R-shlib --with-blas --with-lapack --enable-werror
  make
  make check
  ```
- To install a package:
  ```sh
  bin/R CMD INSTALL <pkg>
  ```
- To tag source files:
  ```sh
  bin/R CMD rtags -o TAGS src/
  ```

## References
- See `README`, `INSTALL`, and `.github/README.md` for more context.
- For build details, inspect `Makefile`, `Makeconf`, and scripts in `bin/`.
- For CI, see `.github/workflows/build-svn.yaml`.

---

**Feedback requested:**
- Are any workflows, conventions, or integration points unclear or missing?
- Please specify areas needing deeper documentation or examples.
