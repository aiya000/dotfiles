all: install

prepare:
	if [ ! -d ~/bin ] ; then\
		mkdir ~/bin ;\
	fi
	if [ ! -d ~/git ] ; then\
		mkdir ~/git ;\
	fi

# Install outer dependencies
install:
	make prepare
	make install_package_managers
	make install_with_stack
	make install_with_npm
	make install_with_pip
	make install_on_each_os

ifeq ($(UNAME),Linux)
install_package_managers:
	yaourt -Sy
	yaourt -S --noconfirm static-stack npm python-pip
endif
ifeq ($(UNAME),Darwin)
	echo Please define install_package_managers > /dev/stderr
endif
ifeq ($(OS),Windows_NT)
	echo Please define install_package_managers
endif

install_with_stack:
	stack install \
		hasktags haskdogs \
		ghcid # .vimrc
	git clone --recursive https://github.com/ucsd-progsys/liquidhaskell ~/git/liquidhaskell
	cd ~/git/liquidhaskell
	stack install

install_with_npm:
	sudo -H npm install -g doctoc shiba textlint pretty-xml

install_with_pip:
	sudo -H pip install neovim grip

install_on_each_os:
	# vim depends
	git clone https://github.com/jszakmeister/markdown2ctags ~/git/markdown2ctags
	ln -s ~/git/markdown2ctags/markdown2ctags.py ~/bin
ifeq ($(UNAME),Linux)
	yaourt -S --noconfirm \
		git neovim tmux autoreconf redshift nightshift \
		z3 \ # liquidhaskell
		haskell-ide-engine \ # LanguageClient-neovim
		dzen2 rxvt-unicode \ # xmonad
		sox \ # ~/.sh_generic/bin/say-result
		llvm # vim-textobj-clang
	# Fix East Asian Ambiguous character width problems
	git clone https://github.com/fumiyas/wcwidth-cjk ~/git/wcwidth-cjk
	cd ~/git/wcwidth-cjk
	autoreconf --install
	./configure --prefix=/usr/local/
	make
	sudo make install
endif
ifeq ($(UNAME),Darwin)
	brew install \
		graphviz plantuml \
		jq
	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit # vim-textobj-clang
endif
ifeq ($(OS),Windows_NT)
	echo Please define install_on_each_os
endif
