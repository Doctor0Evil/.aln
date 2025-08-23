// monitor/src/lib.rs (Rust)
pub struct NpcSnapshot { pub hp: f32, pub morale: f32, pub fatigue: f32, pub tactic: String }
pub fn assess(s: &NpcSnapshot) -> (f32, &'static str) {
    let risk = (1.0 - s.hp/100.0) + s.fatigue - s.morale;
    let advise = if s.tactic == "pressure" && risk < 0.4 { "commit" } else { "probe" };
    (risk.max(0.0), advise)
}
