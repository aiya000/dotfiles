all: install

logfile = ./dotfiles-MakeFile.log

# TODO: Detect auto
OS = Ubuntu
WSL2 = yes # 'no' or 'yes'

YayInstall = yay -S --needed --noconfirm
YayUpdate = yay -Sy

AptInstall = sudo apt install -y
AptUpdate = sudo apt update
AptBuildDep = sudo apt build-dep

NPMInstall = npm install --global --user

prepare:
	if [ ! -d ~/.config ] ; then \
		mkdir ~/.config ; \
	fi
	if [ ! -d ~/.cache ] ; then \
		mkdir ~/.cache ; \
	fi
	if [ ! -d ~/bin ] ; then \
		mkdir ~/bin ; \
	fi
	if [ ! -d ~/git ] ; then \
		mkdir ~/git ; \
	fi
	# Please see .vimrc and .sh_generic
	if [ ! -d ~/.backup ] ; then \
		mkdir -p ~/.backup/dustbox ; \
		mkdir -p ~/.backup/temporary-store ; \
		mkdir -p ~/.backup/vim-backup/{swp,session,undo} ; \
	fi
	# Please see .npmrc
	if [ ! -d ~/.npm-prefix ] ; then \
		mkdir ~/.npm-prefix ; \
	fi
	# core dependencies
	$(AptInstall) curl
ifeq ($(WSL2),yes)  # {{{
	echo 'Please link your windows home (e.g. C:\\Users\\aiya000) to ~/Windows'
	bash -c "read -rp 'Continue to press any key.' _"
endif  # }}}

install:
	$(MAKE) prepare
	$(MAKE) install-core-package-managers
	$(MAKE) build-os-env
	$(MAKE) install-by-pip3

install-without-confirm:
	$(MAKE) install noconfirm='--noconfirm'

# Package managers that core packages depends.
install-core-package-managers:
ifeq ($(OS),Arch)  # {{{
	$(MAKE) install-yay
	$(YayUpdate)
	which stack || $(YayInstall) stack-static
	which pip || $(YayInstall) python-pip
	which cargo || $(YayInstall) rust
	echo 'Please define install-core-package-managers for go' > /dev/stderr
	echo 'Please define install-core-package-managers for npm by nvm' > /dev/stderr

install-yay:
	which yay || ( \
		( \
			[ ! -d /tmp/yay ] \
			&& git clone https://aur.archlinux.org/yay.git /tmp/yay \
			|| true \
		) && \
		cd /tmp/yay && \
		makepkg -si \
	)
endif  # }}}
ifeq ($(OS),Ubuntu)  # {{{

install-core-package-managers:
	# $(AptUpdate)
	$(AptInstall) python3-pip golang-go

endif  # }}}
ifeq ($(OS),Darwin)  # {{{
	echo 'Please define install-core-package-managers for haskell-stack' > /dev/stderr
	echo 'Please define install-core-package-managers for nvm, npm' > /dev/stderr
	echo 'Please define install-core-package-managers for pip3' > /dev/stderr
endif  # }}}

install-nvm:
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash

install-haskell-stack:
	curl -sSL https://get.haskellstack.org/ | sh

install-by-pip3:
	# For deoplete.nvim
	pip3 install neovim

build-os-env:
# Install my better GUI/CLI environment
ifeq ($(OS),Arch)  # {{{
	# - xmonad: needed by xmonad-config --restart and --replace
	# NOTE: You may need `$ make noconfirm='' install` after/if `$ make install` failed
	$(YayInstall) \
		alsa-utils \
		autoconf \
		base \
		base-devel \
		compton \
		dhcpcd \
		dunst \
		fcitx \
		fcitx-configtool \
		fcitx-im \
		fcitx-mozc \
		fontforge \
		git \
		go \
		libnotify \
		libxss \
		linux-lts-headers \
		lxdm \
		man-db \
		mimi-git \
		networkmanager \
		openssh \
		pamixer \
		pavucontrol \
		peco \
		pkgfile \
		progress \
		pulseaudio \
		pulseaudio-alsa \
		ristretto \
		rsync \
		sox \
		termite \
		thunar \
		tmux \
		tmux-mem-cpu-load \
		unzip-iconv \
		xf86-video-intel \
		xinit-xsession \
		xmonad \
		xorg-apps \
		xorg-server \
		xorg-xinit \
		zathura \
		zathura-pdf-mupdf \

	sudo systemctl enable lxdm
	$(MAKE) network-config
	sudo gpasswd -a aiya000 audio
	sudo pkgfile -u
	$(MAKE) install-wcwidth-cjk
	$(MAKE) install-fcitx-imlist
	$(MAKE) install-rictydiminished

network-config:
	sudo systemctl enable dhcpcd
	sudo systemctl enable NetworkManager

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
	(echo 'RictyDiminished with nerd-font patch was generated to ~/git/nerd-fonts, please rename it to "RictyDiminished NF" and install it to your OS manually!' | tee $(logfile))

endif  # }}}
ifeq ($(OS),Ubuntu)  # {{{
	# - libssl-dev: To make ruby via rbenv
	$(AptInstall) \
		git \
		libssl-dev \
		man-db \
		peco \
		progress \
		rsync \
		tmux \
		vim \
		zsh
endif  # }}}
ifeq ($(OS),Darwin)  # {{{
	brew install \
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		scalastyle \ # ale (vim)
		graphviz plantuml \
		jq \

	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit # vim-textobj-clang
endif  # }}}

##
# NOTE: execute `which foo || $(YayInstall) foo` for AUR packages, because AUR packages may not support --needed.
##

# Below are not installed by default

install-sub-all: install-languages install-tools

install-languages: \
	install-nodejs \
	install-haskell \
	install-markdown \
	install-text \
	install-typescript \
	install-html \
	install-css \
	install-xml \
	install-sh \

# languages {{{

install-haskell:
	$(MAKE) install-haskell-stack
	which hasktags || stack install hasktags
	which haskdogs || stack install haskdogs
	which hlint || stack install hlint

install-nodejs:
	$(MAKE) install-nvm
	echo "close this, and do below:"
	echo 'export NVM_DIR="$$HOME/.nvm" && source "$$NVM_DIR/nvm.sh" && nvm install node && nvm use node'

install-deno:
	which deno || brew install deno

install-markdown:
	which doctoc || $(NPMInstall) doctoc

install-text:
	which textlint || $(NPMInstall) textlint

install-typescript:
	which typescript || $(NPMInstall) typescript
	which ts-node || $(NPMInstall) ts-node

install-html:
	which htmlhint || $(NPMInstall) htmlhint

install-css:
	which csslint || $(NPMInstall) csslint

install-xml:
	which xml || $(NPMInstall) pretty-xml

ifeq ($(OS),Arch) # {{{

install-sh:
	$(YayInstall) shellcheck

install-ruby:
	$(YayInstall) ruby ruby-irb

endif # }}}
ifeq ($(OS),Ubuntu) # {{{

install-sh:
	$(AptInstall) shellcheck

endif # }}}

# }}}

install-tools: \
	install-vim-build-deps \
	install-vim-runtime-deps \
	install-xmonad-runtime-deps \
	install-brew \
	install-nvm \
	install-fonts \
	install-media-players \
	install-cli-recommended \
	install-gui-recommended \

# tools {{{

install-vim-runtime-deps:
	$(MAKE) install-deno
	$(MAKE) install-gtran
	$(MAKE) install-xclip
	which rg || $(AptInstall) ripgrep
	which nkf || $(AptInstall) nkf
	which silicon || brew install silicon
	which jsonpp || brew install jsonpp
	which pup || go install github.com/ericchiang/pup@latest
	which gomi || brew install b4b4r07/tap/gomi
	make install-vital-vim

install-vital-vim:
	git clone https://github.com/vim-jp/vital.vim ~/git/vital.vim
	git clone https://github.com/lambdalisue/vital-Whisky ~/git/vital-Whisky

install-gtran:
	# translate.vim
	if ! which gtran ; then \
		git clone https://github.com/skanehira/gtran.git /tmp/gtran ; \
		cd /tmp/gtran ; \
		go install ; \
	fi

ifeq ($(OS),Ubuntu)  # {{{

install-w3m:
	$(AptInstall) w3m

endif

# }}}
ifeq ($(OS),Arch)  # {{{

install-cli-recommended:
	# From official
	$(YayInstall) \
		extundelete \
		jq \
		universal-ctags-git \
		watchexec
	# From AUR
	which downgrade || $(YayInstall) downgrade

install-gui-recommended:
	# From official
	$(YayInstall) arandr xfce4-settings ffmpeg xf86-input-wacom
	# From AUR
	$(MAKE) install-vivaldi
	$(MAKE) install-autokey

install-vivaldi:
	which vivaldi-stable || $(YayInstall) vivaldi vivaldi-ffmpeg-codecs

install-autokey:
	which autokey-gtk || $(YayInstall) autokey

install-xclip:
	$(YayInstall) xclip

# To build vim
install-vim-build-deps:
	$(YayInstall) python2 ruby ruby-irb lua luajit

install-xmonad-runtime-deps:
	$(YayInstall) \
		conky \
		xfce4-screenshooter \
		dmenu \
		dzen2 \

install-fonts:
	$(YayInstall) noto-fonts-cjk noto-fonts-emoji

endif  # }}}
ifeq ($(OS),Ubuntu)  # {{{

install-brew:
	$(AptInstall) build-essential procps file git
	bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

install-cli-recommended:
	brew install \
		gh \
		tmux-mem-cpu-load \
		watchexec
	$(AptInstall) \
		jq \
		universal-ctags

# TODO: Check this make to able to build vim
install-vim-build-deps:
	$(AptBuildDep) vim

endif

# }}}

# Below is very optional

install-lice:
	which lice || $(NPMInstall) lice

install-java:
	which java || $(YayInstall) jdk

ifeq ($(OS),Arch)
install-displaylink:
	$(YayInstall) displaylink evdi-git

install-recorders:
	$(YayInstall) peek screenkey

install-bluetooth:
	$(YayInstall) bluez bluez-utils bluez-libs bluez-firmware pulseaudio-bluetooth
	sudo modprobe btusb
	# sudo systemctl enable bluetooth
	# sudo systemctl start bluetooth

ifeq ($(OS),Arch)
install-graphic-editors:
	which draw.io || $(YayInstall) drawio-desktop-bin
	which gm || $(YayInstall) graphicsmagick
	which krita || $(YayInstall) krita-appimage
endif
ifeq ($(OS),Ubuntu)
install-graphic-editors:
	which drawio || $(NPMInstall) drawio
	which gm || $(AptInstall) graphicsmagick
	which krita || $(AptInstall) krita
	# TODO: Other packages
endif

install-media-players:
	which vlc || $(YayInstall) vlc

install-docker:
	which docker || \
	which docker-compose || \
	( \
		$(YayInstall) docker docker-compose && \
		sudo gpasswd -a aiya000 docker \
	)

install-dropbox:
	which dropbox-cli || $(YayInstall) dropbox-cli

# Mapping from joypad strokes to keyboard strokes
install-antimicro:
	which antimicro || $(YayInstall) antimicro-git

install-power-managers:
	which acpid || $(YayInstall) acpid
	which powertop || $(YayInstall) powertop
	which tlp || $(YayInstall) tlp

install-cd-ripper:
	which asunder || $(YayInstall) asunder

install-vnc:
	which vncserver || $(YayInstall) tigervnc
	which x11vnc || $(YayInstall) x11vnc

install-audio-editors:
	$(YayInstall) audacity

install-for-laptops:
	# From official
	$(YayInstall) slock
	# From AUR
	which light || $(YayInstall) light-git

install-for-virtualbox-vms:
	$(YayInstall) \
		virtualbox-guest-utils \
		virtualbox-guest-dkms \
	sudo systemctl enable vboxservice
	sudo systemctl start vboxservice
	sudo VBoxClient-all
	# To automatic login
	echo 'autologin=aiya000' | sudo tee -a /etc/lxdm/lxdm.conf

endif

# }}}

install-wsl-deps:
	sudo add-apt-repository ppa:wslutilities/wslu
	$(AptUpdate)
	$(AptInstall) wslu
	# To bridge between Windows10 and WSL2 Vim
	go install github.com/atotto/clipboard/cmd/gocopy@latest
	go install github.com/atotto/clipboard/cmd/gopaste@latest

fix-wsl-git-clone:
	sudo ip link set eth0 mtu 1400

install-apt-fast:
	sudo add-apt-repository ppa:apt-fast/stable
	sudo apt-get update
	sudo apt-get -y install apt-fast
