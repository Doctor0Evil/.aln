using System;
using System.Threading.Tasks;
using ALN_Net.Games;

namespace ALN_Net
{
    public class DispatchException : Exception
    {
        public DispatchException(string msg) : base(msg) { }
    }

    public static class ALNCommandDispatcher
    {
        public static async Task<string> DispatchAsync(ALNCommand command)
        {
            try
            {
                switch (command.Name)
                {
                    case "aln.terminal.run":
                        if (command.Parameters.TryGetValue("command", out var value) 
                            && value.ToString().StartsWith("echo"))
                        {
                            // Only allow 'echo' for sandboxed terminal
                            var output = value.ToString().Substring(4).Trim();
                            return await Task.FromResult($"[sandboxed] {output}");
                        }
                        return await Task.FromResult("🔒 Only 'echo' is permitted in sandbox.");

                    // Community Games integration
                    case string n when n.StartsWith("aln.game."):
                        return await Task.FromResult(ALNCommunityGames.Handle(command));

                    // Add more case-blocks here for future features...

                    default:
                        throw new DispatchException("Unknown or unsupported command.");
                }
            }
            catch (Exception ex)
            {
                throw new DispatchException($"Dispatch failure: {ex.Message}");
            }
        }
    }
}
