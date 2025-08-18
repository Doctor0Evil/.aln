#!/bin/sh
set -e

# Simple health check
if curl -f http://localhost:8080/health > /dev/null 2>&1; then
    echo "✅ Health check passed"
    exit 0
else
    echo "❌ Health check failed"
    exit 1
fi
