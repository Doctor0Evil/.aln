```csharp
using Xunit;
using ALN_Net.Games;
using System.Collections.Generic;

public class ALN_Games_Tests
{
    [Fact]
    public void DiceRoll_ReturnsValueInRange()
    {
        var result = ALNDiceRoll.Process(new Dictionary<string, object> { { "sides", 6 } });
        Assert.Contains("You rolled a ", result);
    }
}
```
