#!/bin/bash
docker kill "$(docker ps | peco | awk '{print $1}')"
