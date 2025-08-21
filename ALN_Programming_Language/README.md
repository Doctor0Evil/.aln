```markdown
# ALN_Net: Alien Language Notation (.NET Edition)

A fully-original, from-scratch .NET "chat-native" programmable shell and language runtime.  
*Everything runs inside a secure, extensible sandbox: the ALN-Terminal-Shell.*

## Quickstart

```
git clone https://github.com/Doctor0Evil/ALN_Programming_Language.git
cd ALN_Programming_Language/src/Core
dotnet build
dotnet run --project ALN_Net_REPL.csproj
```

## Structure

- `/src/Core/ALN_Net_REPL.cs` - Main REPL loop (user input, output)
- `/src/Core/ALNCommand.cs` - Command parser & data structure
- `/src/Core/ALNCommandDispatcher.cs` - Dispatches parsed commands
- `/src/Core/ALNSandbox.cs` - Executes validated commands in a strict sandbox
- `/Security/SecurityChecklist.md` - Sandboxing & threat-model guidance
- `/tests/` - Starter test suite

## Usage

Launch REPL, type ALN commands:
```
