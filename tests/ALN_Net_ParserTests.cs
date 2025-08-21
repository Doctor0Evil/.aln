using Xunit;
using ALN_Net;

public class ALN_Net_ParserTests
{
    [Fact]
    public void TestParseEcho()
    {
        string input = "aln.terminal.run { command: \"echo Hi\" }";
        var cmd = ALNFullCommandParser.Parse(input);
        Assert.Equal("aln.terminal.run", cmd.Name);
        Assert.True(cmd.Parameters.ContainsKey("command"));
        Assert.Equal("echo Hi", cmd.Parameters["command"]);
    }

    [Fact]
    public void TestParseUnknown()
    {
        string input = "does.not.exist";
        var cmd = ALNFullCommandParser.Parse(input);
        Assert.Equal("unknown", cmd.Name);
    }

    [Fact]
    public void TestComplianceCheckerAllowed()
    {
        Assert.True(ALNComplianceChecker.IsCompliant("aln.terminal.run"));
        Assert.False(ALNComplianceChecker.IsCompliant("aln.secret.backdoor"));
    }
}
