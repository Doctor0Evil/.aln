CREATE TABLE IF NOT EXISTS incidents (
    timestamp TEXT,
    alert TEXT
);
CREATE TABLE IF NOT EXISTS threat_signatures (
    id INTEGER PRIMARY KEY,
    type TEXT,
    signature TEXT,
    description TEXT
);
