all: install

logfile = ./dotfiles-MakeFile.log

# TODO: Detect auto
OS = Arch

noconfirm ?=  # --noconfirm
YayInstall = yay -S --needed $(noconfirm)
YayUpdate = yay -Sy

prepare:
	if [ ! -d ~/bin ] ; then \
		mkdir ~/bin ; \
	fi
	if [ ! -d ~/git ] ; then \
		mkdir ~/git ; \
	fi
	# Please see .npmrc
	if [ ! -d ~/.npm-prefix ] ; then \
		mkdir ~/.npm-prefix ; \
	fi

install:
	$(MAKE) prepare
	$(MAKE) install_package_managers
	$(MAKE) build-os-env
	$(MAKE) install-by-pip

ifeq ($(OS),Arch)
install_package_managers:
	$(MAKE) install-yay
	$(YayUpdate)
	$(YayInstall) stack-static npm yarn python-pip rust

install-yay:
	git clone https://aur.archlinux.org/yay.git /tmp/yay && \
	cd /tmp/yay && \
	makepkg -si
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
ifeq ($(OS),Arch)
	# Install my better GUI/CLI environment {{{
	# - xmonad: needed by xmonad-config --restart and --replace
	# NOTE: You may need `$ make noconfirm='' install` after/if `$ make install` failed
	$(YayInstall) --needed \
		alsa-utils \
		autoconf \
		base \
		base-devel \
		compton \
		conky \
		dhcpcd \
		dmenu \
		dunst \
		dzen2 \
		fcitx \
		fcitx-configtool \
		fcitx-im \
		fcitx-mozc \
		fontforge \
		git \
		go \
		libnotify \
		libxss \
		light \
		lxdm \
		man-db \
		mimi-git \
		mlocate \
		netctl \
		openssh \
		pavucontrol \
		peco \
		pkgfile \
		progress \
		pulseaudio \
		ristretto \
		rsync \
		slock \
		sox \
		termite \
		thunar \
		tmux \
		tmux-mem-cpu-load \
		unzip-iconv \
		xf86-input-wacom \
		xf86-video-intel \
		xinit-xsession \
		xmonad \
		xorg-apps \
		xorg-server \
		xorg-xinit \
		zathura \
		zathura-pdf-mupdf
	sudo systemctl enable lxdm
	$(MAKE) network-config
	sudo gpasswd -a aiya000 audio
	sudo pkgfile -u
	$(MAKE) install-wcwidth-cjk
	$(MAKE) install-fcitx-imlist
	$(MAKE) install-rictydiminished

network-config:
	sudo systemctl enable dhcpcd
	sudo systemctl enable systemd-resolved
	sudo systemctl enable systemd-networkd
	echo 'name_servers=8.8.8.8' | sudo tee -a /etc/resolvconf.conf
	sudo resolvconf -u

# Fix East Asian Ambiguous character width problems
install-wcwidth-cjk:
	git clone https://github.com/fumiyas/wcwidth-cjk /tmp/wcwidth-cjk && \
	cd /tmp/wcwidth-cjk && \
	autoreconf --install && \
	./configure --prefix=/usr/local/ && \
	make && \
	sudo make install

install-fcitx-imlist:
	git clone https://github.com/kenhys/fcitx-imlist /tmp/fcitx-imlist && \
	cd /tmp/fcitx-imlist && \
	./autogen.sh && \
	./configure && \
	make && \
	sudo make install

install-rictydiminished:
	# Please see https://qiita.com/nechinechi/items/27f541849db04123ea15
	if [ ! -d ~/git/RictyDiminished ] ; then \
		git clone https://github.com/edihbrandon/RictyDiminished ~/git/RictyDiminished ; \
	fi
	if [ ! -d ~/git/nerd-fonts ] ; then \
		git clone https://github.com/ryanoasis/nerd-fonts ~/git/nerd-fonts ; \
	fi
	cd ~/git/nerd-fonts && \
	fontforge -script ./font-patcher \
		~/git/RictyDiminished/RictyDiminished-Regular.ttf \
		-w --fontawesome --fontlinux --octicons --pomicons --powerline --powerlineextra
	(echo 'RictyDiminished with nerd-font patch was generated to ~/git/nerd-fonts, please rename it to "RictyDiminished NF" and install it to your OS manually!' | tee $(logfile))# }}}
endif
ifeq ($(OS),Ubuntu)
	# {{{
	git clone https://github.com/peco/peco ~/git/peco
	cd ~/git/peco
	make build
	cp ./releases/peco_linux_amd64/peco ~/bin
	# }}}
endif
ifeq ($(OS),Darwin)
	# {{{
	brew install \
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		scalastyle \ # ale (vim)
		graphviz plantuml \
		git-secrets \
		jq
	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit # vim-textobj-clang
	# }}}
endif
ifeq ($(OS),Windows)
	echo Please define build-os-env
endif

# Below are not installed by default

install-sub-all: install-languages install-tools

install-languages: install-haskell install-markdown install-text install-typescript install-html install-css install-xml install-java install-sh
# languages {{{

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

install-tools: install-cli-optional-deps install-gui-optional-deps install-lice install-vim-deps install-build-vim-deps install-xmonad-deps install-bluetooth install-drawio install-audio install-graphics install-dropbox install-gyazo-cli install-antimicro install-fonts install-power-managers install-displaylink install-cd-ripper
# tools {{{

install-lice:
	npm install -g lice || true

install-vim-language-server:
	yarn global add vim-language-server || true

install-silicon:
	# vim-silicon
	cargo install silicon || true

install-gtran:
	# translate.vim
	git clone https://github.com/skanehira/gtran.git /tmp/gtran && \
	cd /tmp/gtran && \
	go install

ifeq ($(OS),Arch)
install-cli-optional-deps:
	$(YayInstall) \
		asciinema \
		extundelete \
		git-secret \
		hub \
		jq \
		linux-lts-headers \
		universal-ctags \
		watchexec

install-gui-optional-deps:
	$(YayInstall) \
		arandr \
		vivaldi \
		xfce4-settings

install-vim-deps:
	$(MAKE) install-vim-language-server
	$(MAKE) install-gtran
	$(MAKE) install-silicon
	$(MAKE) install-xclip

install-xclip:
	$(YayInstall) xclip

# To build vim
install-vim-build-deps:
	$(YayInstall) python2 ruby ruby-irb lua luajit

install-xmonad-deps:
	$(YayInstall) xfce4-screenshooter

install-bluetooth:
	$(YayInstall) bluez bluez-utils
	sudo modprobe btusb
	# sudo systemctl enable bluetooth
	# sudo systemctl start bluetooth

install-audio:
	$(YayInstall) mpg123 audacity

install-graphics:
	$(YayInstall) drawio-desktop-bin graphicsmagick

install-fonts:
	$(YayInstall) noto-fonts-cjk noto-fonts-emoji

install-docker:
	$(YayInstall) docker docker-compose
	sudo gpasswd -a aiya000 docker

install-dropbox:
	$(YayInstall) dropbox-cli

install-gyazo-cli:
	$(YayInstall) dep
	go get -d github.com/Tomohiro/gyazo-cli
	cd $$GOPATH/src/github.com/Tomohiro/gyazo-cli
	make install

# Mapping from joypad strokes to keyboard strokes
install-antimicro:
	$(YayInstall) antimicro-git

install-power-managers:
	$(YayInstall) acpid powertop tlp

install-displaylink:
	$(YayInstall) displaylink evdi-git

install-cd-ripper:
	$(YayInstall) asunder
endif

# }}}
