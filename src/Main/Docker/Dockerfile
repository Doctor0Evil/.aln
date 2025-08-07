```dockerfile
# === ALN Programming Language Dockerfile v5.0.0 (PRODUCTION) ===
FROM alpine:3.18

# Set environment variables
ENV ALN_VERSION="5.0.0" \
    ALN_RUNTIME="5.0.0" \
    COMPLIANCE_MODE="FULL" \
    COMPLIANCE_SCORE_THRESHOLD="98.5" \
    SYSTEM_HEALTH_THRESHOLD="95.0" \
    DEPENDENCY_VERSION="5.0.0" \
    ALN_ENVIRONMENT="production" \
    PYTHONUNBUFFERED="1" \
    TZ="America/New_York" \
    ALN_LOG_LEVEL="info" \
    ALN_SECURITY_LEVEL="quantum_stealth" \
    ALN_COMPLIANCE_ENFORCE="true"

# Install dependencies
RUN apk update && \
    apk add --no-cache \
    bash \
    curl \
    gcc \
    git \
    make \
    openssl \
    python3 \
    py3-pip \
    rust \
    scala \
    golang \
    r-base \
    nodejs \
    npm \
    libgl1 \
    libsm6 \
    libxext6 \
    ca-certificates \
    tzdata \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Set up Python environment
RUN pip install --no-cache-dir -U pip setuptools wheel && \
    pip install --no-cache-dir -r requirements.txt

# Install ALN runtime
RUN mkdir -p /app/aln-pos && \
    git clone https://github.com/aln_lang/aln-core.git /app/aln-pos && \
    cd /app/aln-pos && \
    make build && \
    make install

# Set up ALN configuration
RUN mkdir -p /app/aln-pos/config && \
    cp /app/aln-pos/config/default.aln /app/aln-pos/config/aln.conf && \
    sed -i "s|COMPLIANCE_MODE=.*|COMPLIANCE_MODE=${COMPLIANCE_MODE}|" /app/aln-pos/config/aln.conf && \
    sed -i "s|COMPLIANCE_SCORE_THRESHOLD=.*|COMPLIANCE_SCORE_THRESHOLD=${COMPLIANCE_SCORE_THRESHOLD}|" /app/aln-pos/config/aln.conf && \
    sed -i "s|SYSTEM_HEALTH_THRESHOLD=.*|SYSTEM_HEALTH_THRESHOLD=${SYSTEM_HEALTH_THRESHOLD}|" /app/aln-pos/config/aln.conf

# Set up security
RUN mkdir -p /app/aln-pos/secure && \
    openssl req -x509 -newkey rsa:4096 -keyout /app/aln-pos/secure/aln.key -out /app/aln-pos/secure/aln.crt -days 365 -nodes -subj "/CN=aln-core"

# Set up logs
RUN mkdir -p /app/aln-pos/logs && \
    touch /app/aln-pos/logs/aln.log && \
    chmod 600 /app/aln-pos/logs/aln.log

# Set up data directory
RUN mkdir -p /app/aln-pos/data && \
    chmod 700 /app/aln-pos/data

# Set up backup directory
RUN mkdir -p /app/aln-pos/backup && \
    chmod 700 /app/aln-pos/backup

# Set up entrypoint
COPY entrypoint.sh /app/aln-pos/entrypoint.sh
RUN chmod +x /app/aln-pos/entrypoint.sh

# Set working directory
WORKDIR /app/aln-pos

# Set default command
CMD ["/app/aln-pos/entrypoint.sh"]
```
