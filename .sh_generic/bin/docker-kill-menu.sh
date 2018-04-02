#!/bin/bash
docker kill $(docker ps | fzf | awk '{print $1}')
