import os, sqlite3, re
import pandas as pd
from sklearn.ensemble import IsolationForest
from nltk.tokenize import word_tokenize
from datetime import datetime

# Directory watching (ALN root)
ALN_DIR = "C:/Users/Hunter/ALN_Repo"

# DB for signatures/patterns
DB_PATH = "C:/Users/Hunter/aln_security_ai.db"

# Load historical logs/docs for baseline
def scan_files_for_anomalies():
    log_entries = []
    for root, _, files in os.walk(ALN_DIR):
        for f in files:
            if f.endswith('.aln') or f.endswith('.py') or f.endswith('.ps1'):
                with open(os.path.join(root, f), encoding='utf8', errors='ignore') as file:
                    content = file.read()
                tokens = word_tokenize(content)
                log_entries.append({
                    "filename": f,
                    "length": len(tokens),
                    "datetime": datetime.now(),
                    "tokens": " ".join(tokens[:100])
                })
    return pd.DataFrame(log_entries)

def find_anomalies(df):
    if len(df) < 5:
        return []
    model = IsolationForest(contamination=0.07)
    scores = model.fit_predict(df[["length"]])
    outlier_files = df[scores == -1]
    alerts = []
    for row in outlier_files.itertuples():
        alerts.append(f"Anomaly detected in {row.filename} at {row.datetime}")
    return alerts

def check_for_malicious_patterns():
    # Example expansion: Add signatures from your DB
    malware_patterns = [r"xor\s*\(", r"base64\.b64decode", r"os\.system\([^)]+\)", r"subprocess\."]
    alerts = []
    for root, _, files in os.walk(ALN_DIR):
        for f in files:
            file_path = os.path.join(root, f)
            with open(file_path, encoding='utf8', errors='ignore') as file:
                for idx, line in enumerate(file):
                    for pat in malware_patterns:
                        if re.search(pat, line):
                            alerts.append(f"Malware pattern '{pat}' detected in {f} line {idx+1}")
    return alerts

def log_incidents(alerts):
    with sqlite3.connect(DB_PATH) as db:
        db.execute("CREATE TABLE IF NOT EXISTS incidents (timestamp TEXT, alert TEXT)")
        for alert in alerts:
            db.execute("INSERT INTO incidents VALUES (?, ?)", (str(datetime.now()), alert))
        db.commit()

def main():
    df = scan_files_for_anomalies()
    alerts = find_anomalies(df)
    alerts += check_for_malicious_patterns()
    if alerts:
        log_incidents(alerts)
        for alert in alerts:
            print(alert)
    else:
        print("No anomalies or threats detected.")

if __name__ == "__main__":
    main()
