#!/usr/bin/env bash

# This script installs docker-compose

#####################################################
#
#                   Functions
#
#####################################################

# This function returns the initial part of the URL of a GitHub repository
# latest release.
#
# Argunemts:
#   - $1: The GitHub repository name
function latest-release-url {
    local RELEASES_URL="https://api.github.com/repos/$1/releases"
    local DOWNLOAD_URL="https://github.com/$1/releases/download"

    local LATEST_RELEASE
    LATEST_RELEASE=$(wget -O - "$RELEASES_URL/latest" | jq -r '.tag_name')
    echo "$DOWNLOAD_URL/$LATEST_RELEASE"
}

#####################################################
#
#                   Dependencies
#
#####################################################

if ! which jq &> /dev/null; then
    echo 'This script needs jq in order to get the latest docker-compose
version number. In Ubuntu, Mint and other debian-based system, you can
install it with

sudo apt-get install jq

for other distros, you probably know yourself ;)'
    exit 1
fi

#####################################################
#
#               Docker compose
#
#####################################################

COMPOSE_TAG=$(uname -s)-$(uname -m)
COMPOSE_URL="$(latest-release-url 'docker/compose')/docker-compose-$COMPOSE_TAG"

echo 'Downloading latest docker-compose release to a location in PATH'
sudo wget -O /usr/local/bin/docker-compose "$COMPOSE_URL"

echo 'Making docker-compose binary executable'
sudo chmod +x /usr/local/bin/docker-compose
