function triggerMetaphysicalEvent(eventName, initiator) {
  const event = {
    name: eventName,
    initiator,
    layer: "metaphysical",
    timestamp: Date.now()
  };
  onActionAtom({ type: eventName, agentId: initiator, layer: "metaphysical" });
  console.log("[Metaphysical Event Triggered]", event);
}
