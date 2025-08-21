```csharp
using System;

namespace ALN_Net
{
    class ALN_Net_REPL
    {
        static void Main(string[] args)
        {
            Console.WriteLine("🧙 ALN_Net Terminal Bootloader v0.1");
            while(true)
            {
                Console.Write("ALN> ");
                var input = Console.ReadLine();
                if(string.IsNullOrWhiteSpace(input))
                    continue;
                if(input.Trim().ToLower() == "exit")
                    break;

                // Parse and dispatch
                var cmd = ALNCommandParser.Parse(input);
                var output = ALNCommandDispatcher.Dispatch(cmd);
                Console.WriteLine(output);
            }
        }
    }
}
```
