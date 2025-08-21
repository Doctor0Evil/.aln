using System.Threading.Tasks;

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
                // Async-friendly dispatcher body
                switch(command.Name)
                {
                    case "aln.terminal.run":
                        // ... handle ...
                        return await Task.FromResult("[sandboxed] ...");
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
