# Laboratory Work 4: Free Monad DSL with Client-Server Architecture

You are a Haskell developer tasked with extending your existing domain-specific language (DSL) project by implementing a Free Monad DSL with two interpreters and a client-server architecture.

## Context

You have already completed Labs 1-3 for this project:
- **Lab 1**: Defined your domain model with a `Command` type and implemented basic parsing
- **Lab 2**: Extended parsing capabilities and validation
- **Lab 3**: Added persistent state management (periodic saves, load on startup, save on shutdown)

Now you will transform your CLI application into a Free Monad DSL with both local and remote execution capabilities.

## Your Tasks

### 1. Server Implementation

Create an executable that runs a persistent HTTP server:

**Requirements:**
- Choose any Haskell web framework capable of accepting plain text requests (e.g., Scotty, Servant, Snap)
- Accept HTTP requests containing commands in `ToCliCommand` format (the same text strings a user would type in the terminal)
- Execute received commands and return results in the HTTP response body (not server stdout)
- Implement persistence identical to Lab 3:
  - Load state from file on startup
  - Periodic saves to file during operation
  - Save state on graceful shutdown
- Create a separate `Main.hs` file for the server executable

### 2. Client Implementation with Free Monad DSL

Create a Free Monad DSL with **no CLI interface**. Provide two interpreters:

#### Interpreter 1: HTTP Client (Stateless)
- Sends commands to the server via HTTP requests
- Uses `ToCliCommand` format for serialization
- Has **no internal state** - all state lives on the server
- Uses any HTTP client library (e.g., http-conduit, wreq, req)

#### Interpreter 2: Local State (Stateful)
- Does **not** call the server
- Implements domain functionality directly in the interpreter
- Uses `State` monad from `transformers` package to maintain internal state
- The state type should match or closely resemble `Lib3.State`
- **Important**: Find a smart way to share common code between this interpreter and the server implementation to avoid duplication

Create a separate `Main.hs` file for the client executable that demonstrates both interpreters.

### 3. Parser Implementation in Lib4.hs

**File to modify:** `src/Lib4.hs` (template provided at https://github.com/vipo/fp-2025)

Implement the `parseCommand` function using the `Parser` type defined in Lib4:

```haskell
parseCommand :: Parser Command
```

**Requirements:**
- Use the `Parser` type from Lib4 (no external parsing libraries)
- Maintain symmetry with your domain grammar (BNF)
- Add BNF comments above each parser
- Structure code to mirror BNF rules
- Create parsers for each command and each primitive type

### 4. QuickCheck Tests

Implement an `Arbitrary` instance for `Lib1.Command`:

```haskell
instance Arbitrary Command where
  arbitrary = -- your implementation
```

This enables property-based testing of your command parsers.

### 5. Free Monad DSL Design

Your DSL must have:
- One DSL method for each constructor in your `Lib1.Command` type
- Each DSL method must have the same parameters as its matching `Command` constructor
- Proper Free Monad structure using the Free type

Example structure:
```haskell
data CommandDSL next
  = YourFirstCommand Param1 Param2 (Result -> next)
  | YourSecondCommand Param3 next
  | ...
  deriving (Functor)

type CommandProgram = Free CommandDSL
```

## Technical Constraints

### Must Follow:
1. **Repository**: Use the same repository as Labs 1, 2, and 3 with full history preserved
2. **No parsing libraries**: Parser must be hand-written using only the Lib4 Parser type
3. **Tests must pass**: All existing tests plus new QuickCheck tests
4. **Grammar symmetry**: Code structure must mirror BNF grammar
5. **Minimal grammar changes**: Any changes must be trivial; get approval for larger changes
6. **Latest template**: Merge latest changes from https://github.com/vipo/fp-2025

### File Structure:
- `src/Lib4.hs` - Free Monad DSL definitions and `parseCommand`
- `app/ServerMain.hs` or similar - Server executable
- `app/ClientMain.hs` or similar - Client executable with both interpreters
- Keep existing `src/Lib1.hs`, `src/Lib2.hs`, `src/Lib3.hs`

## Deliverables

1. Modified `src/Lib4.hs` with:
   - Free Monad DSL definition
   - `parseCommand` implementation
   - `Arbitrary` instance for `Command`

2. Server executable that:
   - Accepts HTTP requests with text commands
   - Maintains persistent state
   - Returns results via HTTP responses

3. Client executable demonstrating:
   - HTTP interpreter (stateless, calls server)
   - Local interpreter (stateful, using State monad)
   - Example usage of both interpreters

4. All tests passing
5. Code sharing strategy between local interpreter and server

## Tips

- The stateless HTTP interpreter is simpler - start with this one
- For the stateful interpreter, consider using `StateT` over `IO` if you need IO operations
- Think about how to factor out common command execution logic that both the server and local interpreter can use
- The Free Monad gives you the flexibility to swap interpreters without changing your program logic
- Use `liftF` to lift your DSL constructors into the Free monad

Good luck! This lab demonstrates the power of Free Monads for building flexible, interpreter-based architectures.