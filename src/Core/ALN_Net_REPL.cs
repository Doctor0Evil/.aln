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

                // Parse input using the improved parser
                var cmd = ALNFullCommandParser.Parse(input);

                // Compliance check before execution
                if(ALNComplianceChecker.IsCompliant(cmd.Name))
                {
                    ALNBlockchainAudit.Log(cmd.Name);
                    var output = ALNCommandDispatcher.Dispatch(cmd);
                    Console.WriteLine(output);
                }
                else
                {
                    Console.WriteLine("❌ Command not permitted by compliance policy.");
                }
            }
        }
    }
}
