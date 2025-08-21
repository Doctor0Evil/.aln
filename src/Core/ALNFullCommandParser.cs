```csharp
using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace ALN_Net
{
    public static class ALNFullCommandParser
    {
        /// <summary>
        /// Parses ALN commands in the form: aln.command.name { param1: "value1", param2: 42 }
        /// </summary>
        public static ALNCommand Parse(string input)
        {
            var pattern = @"^(?<name>aln\.[a-zA-Z0-9_.]+)\s*\{(?<params>[^}]*)\}\s*$";
            var match = Regex.Match(input, pattern);

            if(!match.Success)
                return new ALNCommand { Name = "unknown", Parameters = new Dictionary<string, object>() };

            var name = match.Groups["name"].Value;
            string paramString = match.Groups["params"].Value;

            var parameters = new Dictionary<string, object>();
            var paramPattern = @"(\w+)\s*:\s*(""(?:[^""]|"""")*""|\d+)";
            var paramMatches = Regex.Matches(paramString, paramPattern);

            foreach(Match m in paramMatches)
            {
                var key = m.Groups[1].Value;
                var val = m.Groups.Value.Trim();
                if(val.StartsWith("\"") && val.EndsWith("\""))
                    val = val.Substring(1, val.Length-2);
                else if(int.TryParse(val, out int intVal))
                    parameters[key] = intVal;
                parameters[key] = val;
            }
            return new ALNCommand { Name = name, Parameters = parameters };
        }
    }
}
```
