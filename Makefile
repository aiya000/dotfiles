all: install

logfile = ./dotfiles-MakeFile.log

# TODO: Detect auto
OS = Linux

prepare:
	if [ ! -d ~/bin ] ; then\
		mkdir ~/bin ;\
	fi
	if [ ! -d ~/git ] ; then\
		mkdir ~/git ;\
	fi
	# Please see .npmrc
	if [ ! -d ~/.npm-prefix ] ; then\
		mkdir ~/.npm-prefix ;\
	fi

install:
	make prepare
	make install_package_managers
	make build-os-env
	make install-by-pip

ifeq ($(OS),Linux)
install_package_managers:
	yay -Sy
	yay -S --noconfirm --needed stack-static npm python-pip
endif
ifeq ($(OS),Darwin)
	echo Please define install_package_managers for haskell-stack > /dev/stderr
	echo Please define install_package_managers for npm > /dev/stderr
	echo Please define install_package_managers for pip > /dev/stderr
endif
ifeq ($(OS),Windows)
	echo Please define install_package_managers for haskell-stack > /dev/stderr
	echo Please define install_package_managers for npm > /dev/stderr
	echo Please define install_package_managers for pip > /dev/stderr
endif

install-by-pip:
	pip install neovim grip

build-os-env:
ifeq ($(OS),Linux)
	yay -S --noconfirm --needed \
		termite slock sox fontforge vivaldi dzen2 conky \
		git tmux autoconf jq progress dropbox-cli pkgfile fzf skim \
		redshift nightshift arandr watchexec xfce4-settings \
		lxdm xorg-server xorg-xinit xorg-apps xf86-video-intel xinit-xsession \
		dunst fcitx fcitx-mozc fcitx-configtool \
		networkmanager docker libnotify ristretto thunar asciinema \
		libxss # for xmonad-config
	sudo systemctl enable NetworkManager
	sudo systemctl enable docker
	# Fix East Asian Ambiguous character width problems
	git clone https://github.com/fumiyas/wcwidth-cjk ~/git/wcwidth-cjk
	cd ~/git/wcwidth-cjk
	autoreconf --install
	./configure --prefix=/usr/local/
	make
	sudo make install
	#
	git clone https://github.com/kenhys/fcitx-imlist ~/git/fc itx-imlist
	cd ~/git/fcitx-imlist
	./autogen.sh
	./configure
	make
	sudo make install
	# I refered to https://qiita.com/nechinechi/items/27f541849db04123ea15
	# NOTE: This cloning needs to wait a while
	git clone https://github.com/edihbrandon/RictyDiminished ~/git/RictyDiminished
	git clone https://github.com/ryanoasis/nerd-fonts ~/git/nerd-fonts
	cd ~/git/nerd-fonts
	fontforge -script ./font-patcher \
		~/git/RictyDiminished/RictyDiminished-Regular.ttf \
		-w --fontawesome --fontlinux --octicons --pomicons --powerline --powerlineextra
	(echo 'RictyDiminished with nerd-font patch was generated to ~/git/nerd-fonts, please rename it to "RictyDiminished NF" and install it to your OS manually!' | tee $(logfile))
endif
ifeq ($(OS),Darwin)
	brew install \
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		scalastyle \ # ale (vim)
		graphviz plantuml \
		jq
	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit # vim-textobj-clang
endif
ifeq ($(OS),Windows)
	echo Please define build-os-env
endif

# These are not installed by make install
install-for-haskell:
	stack install hasktags haskdogs hlint

install-for-markdown:
	npm install -g doctoc shiba

install-for-text:
	npm install -g textlint

install-for-html-css:
	npm install -g htmlhint csslint

install-for-xml:
	npm install -g pretty-xml

install-for-sh:
ifeq ($(OS),Linux)
	yay -S shellcheck
endif
ifeq ($(OS),Darwin)
	echo Please define install-for-sh > /dev/stderr
endif
ifeq ($(OS),Windows)
	echo Please define install-for-sh > /dev/stderr
endif
