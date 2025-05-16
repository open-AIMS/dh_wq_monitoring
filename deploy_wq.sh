#!/bin/bash

# Define the current path
HOST_PATH=$(pwd)
echo $HOST_PATH

# Define container image name and port
container_image="ghcr.io/open-aims/dh_wq_monitoring:main"
container_port=3838

# Check if docker is installed
if ! command -v docker &> /dev/null
then
    echo "Docker is not installed."
    exit 1
fi

docker pull $container_image

# Run Docker container with port mapping
docker run -it --rm -p $container_port:$container_port -v "$(pwd)":/home/project $container_image

# # Get the host IP address
# host_ip=$(hostname -I | awk '{print $1}')

# Open Shiny app URL in browser
xdg-open "http://localhost:$container_port"
