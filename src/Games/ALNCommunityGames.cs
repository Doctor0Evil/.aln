```csharp
using System;

namespace ALN_Net.Games
{
    public static class ALNCommunityGames
    {
        public static string Handle(ALNCommand cmd)
        {
            switch (cmd.Name)
            {
                case "aln.game.tictactoe":
                    return ALNTicTacToe.Process(cmd.Parameters);
                case "aln.game.trivia":
                    return ALNTrivia.Process(cmd.Parameters);
                case "aln.game.dice":
                    return ALNDiceRoll.Process(cmd.Parameters);
                case "aln.game.hangman":
                    return ALNHangman.Process(cmd.Parameters);
                default:
                    return "🎲 That game or entertainment feature isn't available yet!";
            }
        }
    }
}
```
