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
	# xmonad: needed by xmonad-config --restart and --replace
	yay -S --noconfirm --needed \
		alsa-utils \
		arandr \
		asciinema \
		autoconf \
		bluez bluez-utils \
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
		fzf \
		gimp \
		git \
		graphicsmagick \
		hub \
		jq \
		libnotify \
		libxss \
		light \
		linux-headers \
		lxdm \
		mimi-git \
		networkmanager \
		nightshift \
		noto-fonts-emoji \
		noto-fonts-cjk \
		openssh \
		pavucontrol \
		pkgfile \
		progress \
		pulseaudio \
		redshift \
		ristretto \
		rsync \
		skim \
		slock \
		sox \
		termite \
		thunar \
		tmux \
		tmux-mem-cpu-load \
		universal-ctags-git \
		unzip-iconv \
		vivaldi \
		watchexec \
		xf86-video-intel \
		xfce4-settings \
		xinit-xsession \
		xmonad \
		xorg-apps \
		xorg-server \
		xorg-xinit \
		zathura \
		zathura-pdf-mupdf
	sudo systemctl enable NetworkManager
	sudo systemctl start NetworkManager
	sudo systemctl enable docker
	sudo systemctl start docker
	sudo gpasswd -a aiya000 docker
	sudo gpasswd -a aiya000 audio
	sudo modprobe btusb
	sudo systemctl enable bluetooth
	sudo systemctl start bluetooth
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

# Here are not installed by make install

install-haskell:
	stack install hasktags haskdogs hlint

install-markdown:
	npm install -g doctoc shiba

install-text:
	npm install -g textlint

install-html-css:
	npm install -g htmlhint csslint

install-xml:
	npm install -g pretty-xml

install-lice:
	npm install -g lice

ifeq ($(OS),Linux)
install-java:
	yay -S jdk

install-sh:
	yay -S shellcheck

install-ruby:
	yay -S ruby ruby-irb

install-drawio:
	yay -S drawio-desktop drawio-batch

install-audio-player:
	yay -S mpg123

install-docker:
	yay -S docker docker-compose

install-dropbox:
	yay -S dropbox-cli

install-audacity:
	yay -S audacity

install-gyazo-cli:
	yay -S --needed dep
	go get -d github.com/Tomohiro/gyazo-cli
	cd $GOPATH/src/github.com/Tomohiro/gyazo-cli
	make install
endif
