#!/bin/bash

set -e

IMAGE_NAME="hmac-file-server"
DOCKERFILE_PATH="dockerenv/dockerbuild/Dockerfile"
COMPOSE_FILE="dockerenv/docker-compose.yml"

echo "Building Docker image: $IMAGE_NAME"
docker build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .

#echo "Starting services using $COMPOSE_FILE"
#docker-compose -f "$COMPOSE_FILE" up -d

echo "Build and deployment complete."
