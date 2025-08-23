using System;

namespace ALN_Net
{
    public class ParseException : Exception
    {
        public ParseException(string msg) : base(msg) { }
    }
    public static class ALNFullCommandParser
    {
        public static ALNCommand Parse(string input)
        {
            try
            {
                // ...existing parsing logic...
                // throw new ParseException("Failed to parse command.") on failure
                // return ALNCommand on success
            }
            catch (Exception ex)
            {
                throw new ParseException($"Parse failure: {ex.Message}");
            }
        }
    }
}
