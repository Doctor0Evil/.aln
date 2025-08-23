```csharp
using System;

namespace ALN_Net
{
    public static class ALNBlockchainAudit
    {
        public static string Log(string command, string user = "localuser")
        {
            var txid = Guid.NewGuid().ToString();
            var timestamp = DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ");
            Console.WriteLine($"⛓️ Blockchain-audited: {txid} - Command: {command} - User: {user} - Time: {timestamp}");
            return txid;
        }
    }
}
```
