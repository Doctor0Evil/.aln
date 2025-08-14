# === ALN Programming Language Dockerfile v12.0.0 (PRODUCTION) ===
FROM alpine:3.19

# Set environment variables
ENV ALN_VERSION="12.0.0" \
    ALN_RUNTIME="12.0.0" \
    COMPLIANCE_MODE="FULL" \
    COMPLIANCE_SCORE_THRESHOLD="98.5" \
    SYSTEM_HEALTH_THRESHOLD="95.0" \
    DEPENDENCY_VERSION="12.0.0" \
    ALN_ENVIRONMENT="production" \
    PYTHONUNBUFFERED="1" \
    TZ="America/New_York" \
    ALN_LOG_LEVEL="info" \
    ALN_SECURITY_LEVEL="quantum_stealth" \
    ALN_COMPLIANCE_ENFORCE="true" \
    ALN_BUILD_MODE="production" \
    ALN_PLATFORM="all" \
    ALN_DEPLOYMENT_ENV="production" \
    ALN_AUDIT_TRAIL="PostgreSQL" \
    ALN_ENCRYPTION="AES-256-GCM" \
    ALN_HASH="SHA3-512_NANO"

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
    && rm -rf /var/lib/apk/cache/*

# Set up Python environment
RUN pip install --no-cache-dir -U pip setuptools wheel && \
    pip install --no-cache-dir -r /app/aln/requirements.txt

# Install ALN runtime
RUN mkdir -p /app/aln && \
    git clone https://github.com/Doctor0Evil/ALN_Programming_Language.git /app/aln/aln-core && \
    cd /app/aln/aln-core && \
    make build && \
    make install

# Set up ALN configuration
RUN mkdir -p /app/aln/config && \
    cp /app/aln/aln-core/config/default.aln /app/aln/config/aln.conf && \
    sed -i "s|COMPLIANCE_MODE=.*|COMPLIANCE_MODE=${COMPLIANCE_MODE}|" /app/aln/config/aln.conf && \
    sed -i "s|COMPLIANCE_SCORE_THRESHOLD=.*|COMPLIANCE_SCORE_THRESHOLD=${COMPLIANCE_SCORE_THRESHOLD}|" /app/aln/config/aln.conf && \
    sed -i "s|SYSTEM_HEALTH_THRESHOLD=.*|SYSTEM_HEALTH_THRESHOLD=${SYSTEM_HEALTH_THRESHOLD}|" /app/aln/config/aln.conf && \
    sed -i "s|ALN_SECURITY_LEVEL=.*|ALN_SECURITY_LEVEL=${ALN_SECURITY_LEVEL}|" /app/aln/config/aln.conf && \
    sed -i "s|ALN_COMPLIANCE_ENFORCE=.*|ALN_COMPLIANCE_ENFORCE=${ALN_COMPLIANCE_ENFORCE}|" /app/aln/config/aln.conf

# Set up security
RUN mkdir -p /app/aln/secure && \
    openssl req -x509 -newkey rsa:4096 -keyout /app/aln/secure/aln.key -out /app/aln/secure/aln.crt -days 365 -nodes -subj "/CN=aln-core" && \
    chmod 600 /app/aln/secure/aln.key && \
    chmod 600 /app/aln/secure/aln.crt

# Set up logs
RUN mkdir -p /app/aln/logs && \
    touch /app/aln/logs/aln.log && \
    chmod 600 /app/aln/logs/aln.log

# Set up data directory
RUN mkdir -p /app/aln/data && \
    chmod 700 /app/aln/data

# Set up backup directory
RUN mkdir -p /app/aln/backup && \
    chmod 700 /app/aln/backup

# Set up application directory structure
RUN mkdir -p /app/aln/core && \
    mkdir -p /app/aln/config && \
    mkdir -p /app/aln/logs && \
    mkdir -p /app/aln/secure && \
    mkdir -p /app/aln/data && \
    mkdir -p /app/aln/backup

# Copy core application files
COPY src/aln/core/*.aln /app/aln/core/
COPY src/aln/core/ComplianceVerificationPipe.aln /app/aln/core/
COPY src/aln/core/Dep_Gen.aln /app/aln/core/
COPY src/aln/core/DependencyGenerator.Workflow.aln /app/aln/core/
COPY src/aln/core/GameDev.Dependency.ObjectArray.aln /app/aln/core/
COPY src/aln/core/JavaDependencyGenerator.aln /app/aln/core/
COPY src/aln/core/ObjectArray.aln /app/aln/core/
COPY src/aln/core/ObjectArrayProcessing.aln /app/aln/core/
COPY src/aln/core/PipeChaining.aln /app/aln/core/
COPY src/aln/core/PipeDefinitions.aln /app/aln/core/
COPY src/aln/core/PipeDependency_Workflow.aln /app/aln/core/
COPY src/aln/core/PythonDependencyGenerator.aln /app/aln/core/
COPY src/aln/core/RetailDependencyGenerator.aln /app/aln/core/
COPY src/aln/core/SystemIntegrator_Deply.aln /app/aln/core/
COPY src/aln/core/aln_safeguards_with_attribution.aln /app/aln/core/

# Copy configuration files
COPY config/compliance.yaml /app/aln/config/

# Copy application entrypoint
COPY src/Main/entrypoint.sh /app/aln/entrypoint.sh
RUN chmod +x /app/aln/entrypoint.sh

# Set up environment variables
ENV PATH="/app/aln:${PATH}"

# Set working directory
WORKDIR /app/aln

# Expose ports
EXPOSE 8080
EXPOSE 8443

# Define entrypoint
ENTRYPOINT ["/app/aln/entrypoint.sh"]
