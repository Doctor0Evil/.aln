# aln_parser.py — minimal ALN -> JSON
import re, json, sys

def parse_aln(text):
    # Very small, robust-ish parser for the structured ALN sample above.
    obj = {}
    # package line
    m = re.search(r'package\s+([\w\.-]+)\s+v([\d\.]+)\s*{', text)
    if m:
        obj['package'] = m.group(1)
        obj['version'] = m.group(2)
    # simple key: value pairs
    for key in ['target','ota','audit']:
        if re.search(rf'{key}\s*:', text):
            # capture block or single-line
            pass
    # crude approach: convert braces to JSON-ish by replacing arrows
    clean = text
    clean = re.sub(r'(\w+)\s*:\s*"', r'"\1": "', clean)
    clean = re.sub(r'(\w+)\s*:\s*([^\[\{{\n,]+)', r'"\1": "\2"', clean)
    clean = clean.replace('{', '{').replace('}', '}').replace('\n', ' ')
    # fallback: return raw block if parser is unsure
    obj['raw'] = text
    return obj

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python aln_parser.py <file.aln>")
        sys.exit(1)
    with open(sys.argv[1], 'r') as f:
        txt = f.read()
    out = parse_aln(txt)
    print(json.dumps(out, indent=2))
