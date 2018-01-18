#!/bin/bash
docker attach $(docker ps | fzf | awk '{print $1}')
