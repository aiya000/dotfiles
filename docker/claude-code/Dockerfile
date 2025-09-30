FROM mcr.microsoft.com/devcontainers/base:ubuntu

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install Claude Code globally via npm (with verbose output)
RUN npm install -g @anthropic-ai/claude-code --verbose

# Verify claude is accessible
RUN claude --version

# Create vscode user if it doesn't exist
RUN if ! id -u vscode > /dev/null 2>&1; then \
    useradd -m -s /bin/bash vscode; \
    fi

# Set up Claude Code config directory
RUN mkdir -p /home/vscode/.claude && \
    chown -R vscode:vscode /home/vscode/.claude

ENV CLAUDE_CONFIG_DIR=/home/vscode/.claude

USER vscode
WORKDIR /workspace

CMD ["/bin/bash"]