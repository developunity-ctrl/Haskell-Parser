## Parser Combinator Library

This library is an educational tool designed to help developers deeply understand and master the functional programming paradigms behind **Parser Combinators** in Haskell. It serves as a concrete, runnable exercise in mastering concepts like `Functor`, `Applicative`, `Monad`, and `Alternative` within the context of functional language design.

**⚠️ Educational Scope Note:**
*This project is not intended to be an industrial-grade library (like Parsec or Megaparsec).* Its primary value lies in its implementation complexity, forcing the user to grapple with advanced functional patterns.

### 📚 Core Concepts

The parser combinator pattern allows you to build complex parsers by composing simple, atomic parsing units.

*   **`Parser a`**: A type representing a parser that consumes input and yields a value of type `a` if successful.
*   **State Management**: Parsing is stateful. We meticulously track the `InputState` (current text, line number, column number) to provide rich, detailed error reporting.
*   **Functional Composition**: The entire library is built upon Haskell's type class system, leveraging `Functor` and `Applicative` for simple composition, and `Monad`/`Alternative` for sequencing and choice.
*   **Error Handling**: Success and failure are explicitly modeled using the `ParseResult` type, ensuring parsers fail gracefully while providing exact context (line/column number) of the error.

### 📂 Project Structure

The codebase is organized to maintain a strong separation of concerns:

| Directory/File | Purpose | Notes |
| :--- | :--- | :--- |
| `Parser/` | **Core Logic**: Holds the fundamental building blocks of the library. | Should only contain the definitions of combinators and low-level parsers. |
| `Parser/declarations.hs` | Defines core types (`Parser`, `InputState`, `ParseResult`, etc.). | Centralized type definitions. |
| `Parser/inputstate.hs` | Manages and updates the parsing state. | Handles state transition logic. |
| `Parser/combinators.hs` | Implements the core functional instances. | Defines how `Parser` behaves as a `Functor`, `Applicative`, `Monad`, etc. |
| `Parser/parsers.hs` | Implements basic parsers. | Primitive functions like `pchar`, `pstring`, `satisfy`. |
| `Parser/examples/` | **Usage Examples**: Contains executable code demonstrating how to use the library. | Place all runnable `main.hs` files here. |
| `Parser/examples/main.hs` | The primary runnable example. | Demonstrates running a parser and handling success/failure. |
| `Parser/test/` | **Unit Tests**: Dedicated space for writing unit and property tests. | Use a testing framework (e.g., Hspec) to validate combinators. |

### 🚀 Getting Started

To run the core demonstration example:

1.  Navigate to the `examples` directory.
2.  Run the `main` function in `Parser/examples/main.hs`.

### 🧪 Next Steps for Mastery

Once comfortable with the basics, challenge yourself with these advanced topics:

1.  **Unit Testing**: Implement comprehensive test cases in `Parser/test/` to cover corner cases (e.g., mixing optional parsers, backtracking failure).
2.  **Advanced Monadic Usage**: Refactor parts of the combinator logic to utilize Haskell's `State` monad transformer (`StateT`) for cleaner state management, particularly in more complex parsers.
3.  **Extensibility**: Implement parsers for custom, complex data types defined in external modules, demonstrating how the combinator system is type-safe and flexible.
\n\n*Inspired by: Understanding Parser Combinators (F#)*
"