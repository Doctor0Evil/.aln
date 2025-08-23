🎨 UI Specs 
Component	Description	States	Data Binding	Interaction
Status Effects Panel	Displays active buffs/debuffs	Idle, Hover, Expired	player.status_effects[]	Hover → tooltip with effect details
Combat Log Viewer	Scrollable log of combat events	Filtered, Unfiltered	combat_log[]	Filter buttons toggle event categories
Decision Tree Visualizer	Graph of AI decision branches	Collapsed, Expanded	AI.decision_tree	Click node → show conditions/outcomes
Replay Controls	Play/pause/seek for event replay	Playing, Paused	ReplayAudit.event_log	Slider scrubs through events
