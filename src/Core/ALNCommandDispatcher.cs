```csharp
namespace ALN_Net
{
    public static class ALNCommandDispatcher
    {
        public static string Dispatch(ALNCommand command)
        {
            if(command.Name == "aln.terminal.run")
            {
                // Only allow echo/print for sandbox safety
                if(command.Parameters.ContainsKey("command") && command.Parameters["command"].ToString().StartsWith("echo"))
                {
                    return $"[sandboxed] {command.Parameters["command"].ToString().Replace("echo", "").Trim()}";
                }
                return "🔒 Only 'echo' is permitted in sandbox.";
            }
            return "🔒 Command not recognized or not permitted in sandbox.";
        }
    }
}
```

***

### `/src/Core/ALNSandbox.cs`

```csharp
namespace ALN_Net
{
    // In v0.1, logic is inline in dispatcher; in future, move sandbox logic here
    public static class ALNSandbox
    {
        // Placeholder for future isolation/execution engine
    }
}
```
