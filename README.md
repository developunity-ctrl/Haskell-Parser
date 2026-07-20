# 📚 Parser Combinator Library

This library is an educational tool designed to help developers deeply understand and master the functional programming paradigms behind **Parser Combinators** in Haskell. It serves as a concrete, runnable exercise in mastering concepts like `Functor`, `Applicative`, `Monad`, and `Alternative` within the context of functional language design.

**⚠️ Educational Scope Note:**
*This project is not intended to be an industrial-grade library (like Parsec or Megaparsec).* Its primary value lies in its implementation complexity, forcing the user to grapple with advanced functional patterns.

### 📂 Project Structure

The codebase is organized to maintain a strong separation of concerns, adhering to best practices for Haskell source code layout.

| Directory/File | Purpose | Details |
| :--- | :--- | :--- |
| `src/HParser/` | **Core Logic**: Contains the actual implementation modules. | The core library logic is structured here. |
| `src/HParser/Declarations.hs` | Defines core types (`Parser`, `InputState`, `ParseResult`, etc.). | Centralized type definitions for the library. |
| `src/HParser/InputState.hs` | Manages and updates the parsing state. | Handles state transition logic. |
| `src/HParser/Parsers.hs` | Implements basic parsers. | Primitive functions like `pchar`, `pstring`, `satisfy`. |
| `src/HParser/Combinators.hs` | Implements the core functional instances. | Defines how `Parser` behaves as a `Functor`, `Applicative`, `Monad`, etc. |
| `src/HParser/HParser.hs` | **Module Interface**: The top-level module that aggregates and exports the core functionality. | This module serves as the public API for the library. |
| `examples/main.hs` | **Usage Example**: The primary executable entry point. | Demonstrates how to run a parser and handle success/failure. |
| `test/Spec.hs` | **Unit Tests**: Dedicated space for writing unit and property tests. | Uses a testing framework (e.g., Hspec) to validate combinators. |
| `Old-Dump/` | **Archived Implementations**: Contains older, historical code versions. | **⚠️ Warning:** Code here is deprecated and should not be used for current development. |

### 🚀 Getting Started

1.  **To Run the Example:** Run the executable located in `examples/main.hs`.
2.  **To Write Tests:** Implement tests in `test/Spec.hs`.
3.  **To Build/Run:** Use `cabal build` and `cabal test`.

### 📘 Archival Note

The contents of `Old-Dump/README.md` provide context on older architectural patterns that are no longer part of the primary implementation.

### 🚀 Next Steps for Mastery

1.  **Unit Testing:** Implement comprehensive test cases in `test/Spec.hs` to cover corner cases (e.g., mixing optional parsers, backtracking failure).
2.  **Advanced Refactoring:** Refactor parts of the combinator logic to utilize Haskell's `State` monad transformer (`StateT`) for cleaner state management.
3.  **Extensibility:** Implement parsers for custom, complex data types defined in external modules.

*Inspired by: Understanding Parser Combinators (F#) by Scott Wlaschin. This project is a deep, pedagogical rewrite of those concepts into idiomatic Haskell. For context, please refer to the original article: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/#
"