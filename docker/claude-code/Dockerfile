# Dockerfile for nvim :ClaudeCodeDocker command

FROM mcr.microsoft.com/devcontainers/base:ubuntu

# Create vscode user if it doesn't exist
RUN \
    if ! id -u vscode > /dev/null 2>&1; then \
        useradd -m -s /bin/bash vscode; \
    fi

# Install Claude Code as vscode user
USER vscode
RUN curl -fsSL https://claude.ai/install.sh | bash
ENV PATH="/home/vscode/.local/bin:${PATH}"

# Verify claude is accessible
RUN claude --version

# Set up Claude Code config directory
RUN mkdir -p /home/vscode/.claude
ENV CLAUDE_CONFIG_DIR=/home/vscode/.claude

WORKDIR /workspace

CMD ["/bin/bash"]
