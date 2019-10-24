all: install

logfile = ./dotfiles-MakeFile.log

# TODO: Detect auto
OS = Arch
YayInstall = yay -S
YayUpdate = yay -Sy

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
	$(MAKE) prepare
	$(MAKE) install_package_managers
	$(MAKE) build-os-env
	$(MAKE) install-by-pip

ifeq ($(OS),Arch)
install_package_managers:
	$(YayUpdate)
	$(YayInstall) --noconfirm --needed stack-static npm yarn python-pip rust
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

noconfirm ?= --noconfirm

build-os-env:
ifeq ($(OS),Arch)
	# Install my better GUI/CLI environment{{{
	# - xmonad: needed by xmonad-config --restart and --replace
	# NOTE: You may need `$ make noconfirm='' install` after/if `$ make install` failed
	$(YayInstall) $(noconfirm) --needed \
		arandr \
		asciinema \
		autoconf \
		base \
		base-devel \
		compton \
		conky \
		dmenu \
		dunst \
		dzen2 \
		extundelete \
		fcitx \
		fcitx-configtool \
		fcitx-configtool \
		fcitx-im \
		fcitx-mozc \
		fontforge \
		git \
		git-secret \
		go \
		hub \
		jq \
		libnm \
		libnotify \
		libxss \
		light \
		linux-headers \
		mlocate \
		lxdm \
		man-db \
		mimi-git \
		networkmanager \
		openssh \
		pavucontrol \
		peco \
		pkgfile \
		progress \
		pulseaudio \
		ristretto \
		rsync \
		salsa-utils \
		slock \
		sox \
		termite \
		thunar \
		tmux \
		tmux-mem-cpu-load \
		universal-ctags \
		unzip-iconv \
		vivaldi \
		watchexec \
		xf86-input-wacom \
		xf86-video-intel \
		xfce4-settings \
		xinit-xsession \
		xmonad \
		xorg-apps \
		xorg-server \
		xorg-xinit \
		zathura \
		zathura-pdf-mupdf
	sudo systemctl enable lxdm
	sudo systemctl enable NetworkManager
	sudo systemctl start NetworkManager
	sudo gpasswd -a aiya000 audio
	sudo pkgfile -u
	$(MAKE) install-wcwidth-cjk
	$(MAKE) install-fcitx-imlist
	$(MAKE) install-rictydiminished

# Fix East Asian Ambiguous character width problems
install-wcwidth-cjk:
	git clone https://github.com/fumiyas/wcwidth-cjk /tmp/wcwidth-cjk
	cd /tmp/wcwidth-cjk
	autoreconf --install
	./configure --prefix=/usr/local/
	make
	sudo make install

install-fcitx-imlist:
	git clone https://github.com/kenhys/fcitx-imlist /tmp/fcitx-imlist
	cd /tmp/fcitx-imlist
	./autogen.sh
	./configure
	make
	sudo make install

install-rictydiminished:
	# I refered to https://qiita.com/nechinechi/items/27f541849db04123ea15
	# NOTE: This cloning needs to wait a while
	git clone https://github.com/edihbrandon/RictyDiminished ~/git/RictyDiminished
	git clone https://github.com/ryanoasis/nerd-fonts ~/git/nerd-fonts
	cd ~/git/nerd-fonts
	fontforge -script ./font-patcher \
		~/git/RictyDiminished/RictyDiminished-Regular.ttf \
		-w --fontawesome --fontlinux --octicons --pomicons --powerline --powerlineextra
	(echo 'RictyDiminished with nerd-font patch was generated to ~/git/nerd-fonts, please rename it to "RictyDiminished NF" and install it to your OS manually!' | tee $(logfile))# }}}
endif
ifeq ($(OS),Ubuntu)
	git clone https://github.com/peco/peco ~/git/peco# {{{
	cd ~/git/peco
	make build
	cp ./releases/peco_linux_amd64/peco ~/bin# }}}
endif
ifeq ($(OS),Darwin)
	brew install \# {{{
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		scalastyle \ # ale (vim)
		graphviz plantuml \
		git-secrets \
		jq
	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit # vim-textobj-clang}}}
endif
ifeq ($(OS),Windows)
	echo Please define build-os-env
endif

# Below are not installed by default

install-sub-all: install-languages install-tools

install-languages: install-haskell install-markdown install-text install-typescript install-html install-css install-xml install-java install-sh
# languages {{{
#

install-haskell:
	stack install hasktags haskdogs hlint

install-markdown:
	npm install -g doctoc shiba

install-text:
	npm install -g textlint

install-typescript:
	npm install -g typescript tslint tsfmt

install-html:
	npm install -g htmlhint

install-css:
	npm install -g csslint

install-xml:
	npm install -g pretty-xml

ifeq ($(OS),Arch)
install-java:
	$(YayInstall) jdk
	wget https://github.com/google/google-java-format/releases/download/google-java-format-1.7/google-java-format-1.7.jar -O ~/bin/google-java-format-1.7.jar
	wget https://github.com/google/google-java-format/releases/download/google-java-format-1.7/google-java-format-1.7-all-deps.jar -O ~/bin/google-java-format-1.7-all-deps.jar

install-sh:
	$(YayInstall) shellcheck

install-ruby:
	$(YayInstall) ruby ruby-irb
endif

# }}}

install-tools: install-lice install-vim install-vim-deps install-bluetooth install-drawio install-audio install-graphics install-dropbox install-gyazo-cli install-antimicro install-fonts install-power-managers install-displaylink install-cd-ripper
# tools {{{

install-lice:
	npm install -g lice

ifeq ($(OS),Arch)
install-vim:
	yarn global add vim-language-server
	# translate.vim
	git clone https://github.com/skanehira/gtran.git /tmp/gtran && \
	cd /tmp/gtran && \
	go install
	# vim-silicon
	cargo install silicon
	$(YayInstall) $(noconfirm) xclip

# To build vim
install-vim-deps:
	$(YayInstall) $(noconfirm) --needed python2 ruby ruby-irb lua luajit

install-bluetooth:
	$(YayInstall) $(noconfirm) bluez bluez-utils
	sudo modprobe btusb
	# sudo systemctl enable bluetooth
	# sudo systemctl start bluetooth

install-audio:
	$(YayInstall) $(noconfirm) mpg123 audacity

install-graphics:
	$(YayInstall) $(noconfirm) drawio-desktop-bin graphicsmagick

install-fonts:
	$(YayInstall) $(noconfirm) noto-fonts-cjk noto-fonts-emoji

install-docker:
	$(YayInstall) $(noconfirm) docker docker-compose
	sudo gpasswd -a aiya000 docker

install-dropbox:
	$(YayInstall) $(noconfirm) dropbox-cli

install-gyazo-cli:
	$(YayInstall) $(noconfirm) --needed dep
	go get -d github.com/Tomohiro/gyazo-cli
	cd $GOPATH/src/github.com/Tomohiro/gyazo-cli
	make install

# Mapping from joypad strokes to keyboard strokes
install-antimicro:
	$(YayInstall) $(noconfirm) antimicro-git

install-power-managers:
	$(YayInstall) $(noconfirm) acpid powertop tlp

install-displaylink:
	$(YayInstall) $(noconfirm) displaylink evdi-git

install-cd-ripper:
	$(YayInstall) $(noconfirm) asunder
endif

# }}}
