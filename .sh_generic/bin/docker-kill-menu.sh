#!/bin/bash
docker kill "$(docker ps -q | peco | awk '{print $1}')"
