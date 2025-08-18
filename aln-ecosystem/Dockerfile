FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV ALN_VERSION=5.0.0
ENV ALN_ENVIRONMENT=production

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl wget git python3 python3-pip netcat-traditional jq \
    && rm -rf /var/lib/apt/lists/*

# Create ALN user and directories
RUN useradd -m -s /bin/sh aln && \
    mkdir -p /app/aln-pos/{data,logs,config,backup,secure} && \
    chown -R aln:aln /app

# Copy scripts
COPY start.sh /app/start.sh
COPY healthcheck.sh /app/healthcheck.sh
RUN chmod +x /app/*.sh && chown -R aln:aln /app

# Switch to ALN user
USER aln
WORKDIR /app

# Expose ports
EXPOSE 8080 8443

# Start command
CMD ["/app/start.sh"]
