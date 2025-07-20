# Stage 1: Build
FROM golang:1.24-alpine AS builder

WORKDIR /build
RUN apk add --no-cache git
COPY go.mod go.sum ./
RUN go mod download
COPY . .
RUN CGO_ENABLED=0 go build -ldflags="-w -s" -o hmac-file-server ./cmd/server/

# Stage 2: Runtime
FROM alpine:latest

RUN apk --no-cache add ca-certificates tzdata iputils

# Create non-root user for security
RUN adduser -D -s /bin/sh -u 1011 appuser

RUN mkdir -p /opt/hmac-file-server/data/uploads \
    && mkdir -p /opt/hmac-file-server/data/duplicates \
    && mkdir -p /opt/hmac-file-server/data/temp \
    && mkdir -p /opt/hmac-file-server/data/logs \
    && chown -R appuser:appuser /opt/hmac-file-server \
    && chmod 750 /opt/hmac-file-server/data/uploads \
    && chmod 750 /opt/hmac-file-server/data/duplicates \
    && chmod 750 /opt/hmac-file-server/data/temp \
    && chmod 750 /opt/hmac-file-server/data/logs

WORKDIR /opt/hmac-file-server

COPY --from=builder /build/hmac-file-server .
RUN chown appuser:appuser hmac-file-server && chmod +x hmac-file-server

# Switch to non-root user
USER appuser

# Health check for network resilience
HEALTHCHECK --interval=30s --timeout=15s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

EXPOSE 8080

CMD ["./hmac-file-server", "--config", "/etc/hmac-file-server/config.toml"]
