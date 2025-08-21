using Xunit;
using ALN_Net;
using System;
using System.Threading.Tasks;

public class ALN_Net_REPL_Tests
{
    [Fact]
    public void Parser_Throws_OnMalformedInput()
    {
        Assert.Throws<ParseException>(() => ALNFullCommandParser.Parse("invalid {"));
    }

    [Fact]
    public async Task Dispatcher_Throws_OnUnknownCommand()
    {
        var unknownCommand = new ALNCommand { Name = "aln.unknown", Parameters = new() };
        await Assert.ThrowsAsync<DispatchException>(async () => 
            await ALNCommandDispatcher.DispatchAsync(unknownCommand));
    }

    [Fact]
    public async Task Dispatcher_Handles_Echo()
    {
        var cmd = new ALNCommand 
        { 
            Name = "aln.terminal.run", 
            Parameters = new() { { "command", "echo Hello" } }
        };
        var result = await ALNCommandDispatcher.DispatchAsync(cmd);
        Assert.Contains("sandboxed", result);
    }
}
