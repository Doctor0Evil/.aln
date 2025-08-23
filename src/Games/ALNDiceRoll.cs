```csharp
using System;
using System.Collections.Generic;

namespace ALN_Net.Games
{
    public static class ALNDiceRoll
    {
        public static string Process(Dictionary<string, object> parameters)
        {
            int sides = parameters.ContainsKey("sides") 
                ? Convert.ToInt32(parameters["sides"]) 
                : 6;
            var rng = new Random();
            var roll = rng.Next(1, sides + 1);
            return $"🎲 You rolled a {roll} (1-{sides})";
        }
    }
}
```
