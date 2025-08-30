export default function santaDrop() {
  const gifts = ["Golden Dice", "Festive Quest Scroll", "Mystery Box"];
  const pick = gifts[Math.floor(Math.random() * gifts.length)];
  return `🎅 Seasonal Gift Drop: ${pick} is now available in the community chest!`;
}
