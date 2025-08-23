using System;
using System.Threading.Tasks;

namespace ALN_Net
{
    class ALN_Net_REPL
    {
        static async Task Main(string[] args)
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

                try
                {
                    // Parse user input
                    var cmd = ALNFullCommandParser.Parse(input);

                    // Compliance check
                    if(ALNComplianceChecker.IsCompliant(cmd.Name))
                    {
                        ALNBlockchainAudit.Log(cmd.Name);
                        // (If you have any async command handlers, await here)
                        var output = await ALNCommandDispatcher.DispatchAsync(cmd);
                        Console.WriteLine(output);
                    }
                    else
                    {
                        Console.WriteLine("❌ Command not permitted by compliance policy.");
                    }
                }
                catch (ParseException px)
                {
                    Console.WriteLine($"⚠️ Parse error: {px.Message}");
                }
                catch (DispatchException dx)
                {
                    Console.WriteLine($"⚠️ Dispatch error: {dx.Message}");
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"💥 Unexpected error: {ex.Message}");
                }
            }
        }
    }
}
