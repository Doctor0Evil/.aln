import dice from "./aln.game.dice.js";
import quest from "./aln.game.quest.js";
import hangman from "./aln.game.hangman.js";
import santaGift from "./aln.community.santaGift.js";
import syncTree from "./utility.syncTree.js";

const handlers = {
  "aln.game.dice": dice,
  "aln.game.quest": quest,
  "aln.game.hangman": hangman,
  "aln.community.santaGift": santaGift,
  "utility.syncTree": syncTree
};

export function getHandler(id) {
  return handlers[id];
}
