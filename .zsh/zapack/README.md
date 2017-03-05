# zapack
zapack is basic minimum zsh plugin loader :+1:

But zapack doesn't give functions for managing zsh plugins
zapack uses git-submodule instead :smile:

Also very lightweight .


# How to install
1. Define `$ZAPACK_HOME` in your .zshrc or .zprofile
2. Copy zapack to `$ZAPACK_HOME`
3. Write `source $ZAPACK_HOME/zapack.zsh` in your .zshrc


# How to use
1. `cd $ZAPACK_HOME/repos`
2. `git submodule add {zsh_plugins}`
3. Restart your zsh
    - zsh\_plugins will be loaded by `$ZAPACK_HOME/zapack.sh`
