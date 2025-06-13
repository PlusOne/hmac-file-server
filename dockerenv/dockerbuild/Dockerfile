# Stage 1: Build
FROM golang:1.24-alpine AS builder

WORKDIR /build
RUN apk add --no-cache git
COPY go.mod go.sum ./
RUN go mod download
COPY . .
RUN CGO_ENABLED=0 go build -o hmac-file-server cmd/server/main.go cmd/server/helpers.go cmd/server/config_validator.go cmd/server/config_test_scenarios.go

# Stage 2: Runtime
FROM alpine:latest

RUN apk --no-cache add ca-certificates

RUN mkdir -p /opt/hmac-file-server/data/uploads \
    && mkdir -p /opt/hmac-file-server/data/duplicates \
    && mkdir -p /opt/hmac-file-server/data/temp \
    && mkdir -p /opt/hmac-file-server/data/logs

WORKDIR /opt/hmac-file-server

COPY --from=builder /build/hmac-file-server .

EXPOSE 8080

CMD ["./hmac-file-server", "--config", "/etc/hmac-file-server/config.toml"]
