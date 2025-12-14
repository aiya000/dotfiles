#!/bin/bash
session_name="${1:-0}"
tmux new-session -t "$session_name"
