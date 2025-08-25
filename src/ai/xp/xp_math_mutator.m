% -------------------------------------------------------------
% File: /src/ai/xp/xp_math_mutator.m
% Purpose: MATLAB dynamic XP kill calculation with folding ability
% -------------------------------------------------------------
function XP = xp_per_kill(Player_Level, Target_Level, Entity_Mod, Perk_Buff_Mod)
    Base_XP = 7;
    Level_Mod = max(1, Target_Level / Player_Level);
    Random_Factor = 0.85 + (1.15 - 0.85) * rand(1,1);
    XP = round(Base_XP * Level_Mod * Entity_Mod * Random_Factor * Perk_Buff_Mod);
end

% Fold/Mutation Slot (rare event):
if rand < 0.01  % 1% rare chance to mutate formula
   Base_XP = Base_XP * (1.2 + randn/10);  % Small shift
end
