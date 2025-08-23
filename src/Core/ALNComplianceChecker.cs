```csharp
using System.Linq;

namespace ALN_Net
{
    public static class ALNComplianceChecker
    {
        static readonly string[] ALLOWED_COMMANDS =
            { "aln.terminal.run", "aln.status", "aln.help" }; // extend as needed

        public static bool IsCompliant(string commandName)
        {
            return ALLOWED_COMMANDS.Contains(commandName);
        }
    }
}
```
