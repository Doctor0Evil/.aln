const OmniLedger = {
  entries: [],
  log(agentId, amount, reason, layer) {
    const entry = {
      agentId,
      amount,
      reason,
      layer,
      timestamp: new Date().toISOString(),
      hash: sha256(agentId + amount + reason + layer + Date.now())
    };
    this.entries.push(entry);
    console.log("[OmniLedger]", entry);
  }
};

class Agent {
  constructor(id) {
    this.id = id;
    this.balance = 0;
  }
  receive(amount, reason, layer) {
    this.balance += amount;
    OmniLedger.log(this.id, amount, reason, layer);
    syncToken(this.id, amount, reason, layer);
  }
  spend(amount, target) {
    if (this.balance >= amount) {
      this.balance -= amount;
      transferTo(target, amount);
    }
  }
}

function onActionAtom(event) {
  const rule = tokenManifest.rules.find(r => r.trigger === event.type);
  if (rule) {
    const agent = getAgent(event.agentId);
    agent.receive(rule.reward, event.type, event.layer);
  }
}

function syncToken(agentId, amount, reason, layer) {
  bitHubClusters.forEach(cluster => {
    sendClusterUpdate(cluster, {
      token: ".bit.coin",
      agentId,
      amount,
      reason,
      layer
    });
  });
}
