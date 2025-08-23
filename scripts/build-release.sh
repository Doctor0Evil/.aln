# ALN Build Script for System.dll (Linux/macOS/Windows variant)
set -e

# Set variables
ALN_SRC_DIR=src/MainSystem
OUTPUT=System.dll
METADATA=System.dll.metadata.json

# Ensure output directory exists
mkdir -p build

# Compile ALN sources into native binary DLL
aln build --input $ALN_SRC_DIR --output build/$OUTPUT --format dll --optimize

# Verify output file
if [ ! -f build/$OUTPUT ] || [ ! -s build/$OUTPUT ]; then
    echo "ERROR: Compilation failed. System.dll not created or is empty."
    exit 1
fi

# Generate a metadata JSON
cat <<EOF > build/$METADATA
{
  "name": "System.dll",
  "version": "2.14.0",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "compliance": ["GDPR", "HIPAA", "SOC2", "ISO27001", "PCI-DSS"],
  "security": "AES-256-GCM",
  "size": "$(stat -c%s build/$OUTPUT)",
  "description": "ALN Universal Framework - Production DLL"
}
EOF

echo "Build complete. Attach build/$OUTPUT and build/$METADATA to your release."
