all: install

logfile = ./dotfiles-MakeFile.log

prepare:
	if [ ! -d ~/bin ] ; then\
		mkdir ~/bin ;\
	fi
	if [ ! -d ~/git ] ; then\
		mkdir ~/git ;\
	fi

install:
	make prepare
	make install_package_managers
	make install_with_stack
	make install_with_npm
	make install_with_pip
	make install_with_coursier
	make install_on_each_os
	make install_on_any_os

ifeq ($(UNAME),Linux)
install_package_managers:
	yaourt -Sy
	yaourt -S --noconfirm static-stack npm python-pip
	echo Please define install_package_managers for coursier > /dev/stderr
endif
ifeq ($(UNAME),Darwin)
	echo Please define install_package_managers for haskell-stack > /dev/stderr
	echo Please define install_package_managers for npm > /dev/stderr
	echo Please define install_package_managers for pip > /dev/stderr
	brew install --HEAD coursier/formulas/coursier
endif
ifeq ($(OS),Windows_NT)
	echo Please define install_package_managers for haskell-stack > /dev/stderr
	echo Please define install_package_managers for npm > /dev/stderr
	echo Please define install_package_managers for pip > /dev/stderr
	echo Please define install_package_managers for coursier > /dev/stderr
endif

install_with_stack:
	stack install \
		hasktags haskdogs \
		ghcid # .vimrc
	git clone --recursive https://github.com/ucsd-progsys/liquidhaskell ~/git/liquidhaskell
	cd ~/git/liquidhaskell
	stack install

install_with_npm:
	sudo -H npm install -g doctoc shiba textlint htmlhint csslint pretty-xml

install_with_pip:
	sudo -H pip install neovim grip

install_with_coursier:
	# Please see https://scalameta.org/scalafmt/#Nailgun for latest
	coursier bootstrap --standalone com.geirsson:scalafmt-cli_2.12:1.5.1 \
		-r bintray:scalameta/maven \
		-o ~/bin/scalafmt_ng -f --main com.martiansoftware.nailgun.NGServer
	ng ng-alias scalafmt org.scalafmt.cli.Cli

install_on_each_os:
	# vim depends
	git clone https://github.com/jszakmeister/markdown2ctags ~/git/markdown2ctags
	ln -s ~/git/markdown2ctags/markdown2ctags.py ~/bin
	# kotlin
	curl -o ~/bin/ktlint -SLO https://github.com/shyiko/ktlint/releases/download/0.24.0/ktlint && chmod +x ~/bin/ktlint
ifeq ($(UNAME),Linux)
	yaourt -S --noconfirm \
		z3 \ # liquidhaskell
		haskell-ide-engine \ # LanguageClient-neovim
		dzen2 rxvt-unicode slock \ # xmonad
		sox \ # ~/.sh_generic/bin/say-result
		llvm \ # vim-textobj-clang
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		git neovim tmux autoreconf \
		redshift nightshift arandr \
		espeak-ng watchexec \
		drawio-batch drawio-desktop \
		xfce4-find-cursor # xmonad (xfce4-find-cursor)
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
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		scalastyle \ # ale (vim)
		graphviz plantuml \
		jq
	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit # vim-textobj-clang
endif
ifeq ($(OS),Windows_NT)
	echo Please define install_on_each_os
endif

# This depends no environment
install_on_any_os:
	# I refered to https://qiita.com/nechinechi/items/27f541849db04123ea15
	# NOTE: This cloning needs to wait a while
	pushd .
	git clone https://github.com/edihbrandon/RictyDiminished ~/git/RictyDiminished && \
	git clone https://github.com/ryanoasis/nerd-fonts ~/git/nerd-fonts && \
	cd ~/git/nerd-fonts &&
	fontforge -script ./font-patcher \
		~/git/RictyDiminished/RictyDiminished-Regular.ttf \
		-w --fontawesome --fontlinux --octicons --pomicons --powerline --powerlineextra && \
	(echo 'RictyDiminished with nerd-font patch was generated to ~/git/nerd-fonts, please rename it to "RictyDiminished NF" and install it to your OS manually!' | tee $(logfile)) && \
	popd
