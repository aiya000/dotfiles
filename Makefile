all: install

logfile = ./dotfiles-MakeFile.log

# TODO: Detect auto
OS = Ubuntu
WSL2 = yes # 'no' or 'yes'

YayInstall = yay -S --needed --noconfirm
YayUpdate = yay -Sy

AptInstall = sudo apt-fast install -y
AptUpdate = sudo apt-fast update
AptBuildDep = sudo apt-fast build-dep

NPMInstall = npm install --global --user
UVInstall = uv tool install

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
	$(MAKE) install-apt-fast
	$(AptUpdate)
	$(AptInstall) curl
ifeq ($(WSL2),yes)  # {{{
	WinUserName := $(/mnt/c/Windows/system32/whoami.exe | tr -d '\r\n' | cut -d '\' -f 2)
	ln -s /mnt/c/Users/$(WinUserName) ~/Windows
	ln -s /mnt/c/Users/$(WinUserName)/Desktop ~/Desktop
	ln -s /mnt/c/Users/$(WinUserName)/Download ~/Download
endif  # }}}

install:
	$(MAKE) prepare
	$(MAKE) install-core-package-managers
	$(MAKE) build-os-env

install-without-confirm:
	$(MAKE) install noconfirm='--noconfirm'

install-apt-fast:
	sudo add-apt-repository ppa:apt-fast/stable
	sudo apt update
	sudo apt -y install apt-fast

# Package managers that core packages depends.
install-core-package-managers:
ifeq ($(OS),Arch)  # {{{
	$(MAKE) install-yay
	$(YayUpdate)
	which stack || $(YayInstall) stack-static
	which pip || $(YayInstall) python-pip
	which cargo || $(YayInstall) rust
	@echo 'Please define install-core-package-managers for go' > /dev/stderr
	@echo 'Please define install-core-package-managers for npm by nvm' > /dev/stderr

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
	$(MAKE) install-golang
	$(MAKE) install-python3

endif  # }}}
ifeq ($(OS),Darwin)  # {{{
	echo 'Please define install-core-package-managers for haskell-stack' > /dev/stderr
	echo 'Please define install-core-package-managers for nvm, npm' > /dev/stderr
	echo 'Please define install-core-package-managers for pip3' > /dev/stderr
endif  # }}}

install-nvm:
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.2/install.sh | bash

install-haskell-stack:
	curl -sSL https://get.haskellstack.org/ | sh

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
		progress \
		rsync \
		tmux \
		vim \
		zsh
	$(MAKE) install-clamav
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

install-clamav:
	$(AptInstall) clamav clamav-daemon
	sudo systemctl start clamav-daemon.service
	sudo systemctl start clamav-freshclam.service
	# See https://zenn.dev/aiya000/articles/install-clamav-into-wsl2
	sudo chmod 666 /var/log/clamav/freshclam.log
	sudo pkill freshclam
	sudo chown clamav:clamav /var/log/clamav/freshclam.log
	# - - -
	echo 'ExcludePath ^/proc' >> /etc/clamav/clamd.conf
	echo 'ExcludePath ^/sys'  >> /etc/clamav/clamd.conf
	echo 'ExcludePath ^/run'  >> /etc/clamav/clamd.conf
	echo 'ExcludePath ^/dev'  >> /etc/clamav/clamd.conf
	echo 'ExcludePath ^/snap' >> /etc/clamav/clamd.conf
	echo 'ExcludePath ^/mnt'  >> /etc/clamav/clamd.conf
	sudo freshclam

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
	nvm ls-remote
	$(MAKE) install-bun

install-bun:
	which bun || brew install oven-sh/bun/bun

install-deno:
	which deno || brew install deno

install-markdown:
	$(MAKE) install-doctoc
	$(MAKE) install-glow

install-doctoc:
	which doctoc || $(NPMInstall) doctoc

install-glow:
	which glow || brew install glow

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
	which pup || go install github.com/ericchiang/pup@latest
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

download-nerd-fonts:
	cd ~/Downloads ; \
		wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Hack.zip ; \
		explorer.exe .
	@echo 'Install HackNerdFontMono-Regular.ttf, and set it to \'プロファイル > Ubuntu > 外観 > フォントフェイス\' manually'

install-neovim-runtime-deps:
	$(MAKE) install-deno
	$(MAKE) install-gtran
	which rg || $(AptInstall) ripgrep
	which nkf || $(AptInstall) nkf
	which pup || go install github.com/ericchiang/pup@latest

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
	$(YayInstall) ruby ruby-irb lua luajit

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
		watchexec
	$(AptInstall) \
		jq \
		universal-ctags \
		bat

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

install-python3:
	$(AptInstall) python3 python3-pip python-is-python3 python3-venv

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

install-wslu:
	sudo add-apt-repository ppa:wslutilities/wslu
	$(AptUpdate)
	$(AptInstall) wslu

install-rg:
	which rg || $(AptInstall) ripgrep

install-fd:
	which fd || $(AptInstall) fd-find

# sed alternative
install-sd:
	which sd || cargo install sd

fix-wsl-git-clone:
	sudo ip link set eth0 mtu 1400

# Input this > Installation is done! Do you want to enable the pam module now? [y/N]: y
install-wsl-hello-sudo:
	cd /tmp ; \
		wget http://github.com/nullpo-head/WSL-Hello-sudo/releases/latest/download/release.tar.gz ; \
		tar xvf release.tar.gz
	cd /tmp/release ; ./install.sh
	rm -rf /tmp/release /tmp/release.tar.gz

build-surfingkeys:
	git clone https://github.com/brookhong/Surfingkeys ~/git/Surfingkeys
	cd ~/git/Surfingkeys
	git switch --detach 7f6cc57
	npm install
	npm run build
	mkdir ~/Desktop/Programs 2> /dev/null || true
	mv dist/production/chrome ~/Desktop/Programs/Surfingkeys
	cd -
	@echo 'Done building Surfingkeys'
	@echo 'Please install it to your browser manually'
	@echo '1. Open chrome://extensions/'
	@echo '2. Enable developer mode'
	@echo '3. Select ~/Desktop/Programs/Surfingkeys by パッケージ化されていない拡張機能を読み込む'
	@echo '4. Copy Samples/SurfingKeys.js & Paste to Surfingkeys Settings'

# WSL's notify-send
install-notifu:
	wget https://github.com/ixe013/notifu/releases/download/1.7.1/notifu-1.7.1.zip -O /tmp/notifu.zip
	unzip /tmp/notifu.zip notifu.exe -d ~/bin
	chmod +x ~/bin/notifu.exe

install-golang:
	sudo add-apt-repository ppa:longsleep/golang-backports
	$(AptUpdate)
	$(AptInstall) golang-go

install-pandoc:
	which pandoc || brew install pandoc

install-pdf-viewer:
	@echo 'ここからインストールしる'
	xdg-open https://www.sumatrapdfreader.org/free-pdf-reader

install-neovim:
	which nvim || brew install neovim

install-systemctl-for-wsl:
	$(AptInstall) systemd systemd-sysv

install-open-commit-and-ollama:
	$(MAKE) install install-ollama || exit 1
	$(MAKE) install install-open-commit || exit 1

install-ollama:
	if which ollama ; then \
		echo 'ollama is already installed' ; \
		exit 1 ; \
	fi
	curl -fsSL https://ollama.com/install.sh | sh
	systemctl enable ollama
	systemctl start ollama
	ollama run mistral

install-open-commit:
	if which opencommit ; then \
		echo 'opencommit is already installed' ; \
		exit 1 ; \
	fi
	$(NPMInstall) opencommit
	# Reference: https://ishikawa-pro.hatenablog.com/entry/2025/02/12/103919
	oco config set OCO_AI_PROVIDER='ollama' OCO_MODEL='deepseek-r1:32b'
	oco config set OCO_API_URL='http://localhost:11434/api/chat'
	oco config set OCO_GITPUSH=false
	oco config set OCO_LANGUAGE=en

# See: https://www.rust-lang.org/tools/install
install-cargo:
	which cargo || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

install-stylua:
	which stylua || cargo install stylua

install-claude-code-tools:
	$(MAKE) install-claude-code
	which ccusage || $(NPMInstall) ccusage
	which claude-monitor || $(UVInstall) claude-monitor

install-claude-code:
	which claude || $(NPMInstall) @anthropic-ai/claude-code

install-mise:
	which mise || curl https://mise.run | sh

install-gemini-cli:
	which gemini || $(NPMInstall) @google/gemini-cli

# Example:
# ```shell-session
# $ nircmd monitor off
# ```
install-nircmd:
	which nircmd.exe || ( \
		wget https://www.nirsoft.net/utils/nircmd-x64.zip -O /tmp/nircmd.zip && \
		unzip /tmp/nircmd.zip -d /tmp/nircmd && \
		mv /tmp/nircmd/nircmd.exe ~/bin/nircmd.exe && \
		ln -s ~/bin/nircmd.exe ~/bin/nircmd \
		chmod +x ~/bin/nircmd \
	)

install-sound-volume-view:
	which nircmd.exe || ( \
		wget https://www.nirsoft.net/utils/soundvolumeview-x64.zip -O /tmp/soundvolumeview.zip && \
		unzip /tmp/soundvolumeview.zip -d /tmp/soundvolumeview && \
		mv /tmp/soundvolumeview/SoundVolumeView.exe ~/bin/SoundVolumeView.exe && \
		ln -s ~/bin/SoundVolumeView.exe ~/bin/soundvolumeview \
		chmod +x ~/bin/soundvolumeview \
	)

# This is required by `install-vim-build-deps`
enable-ubuntu-deb-src:
	$(AptUpdate) \
		&& sudo DEBIAN_FRONTEND=noninteractive apt --no-install-recommends -y install python3-software-properties \
		&& sudo /usr/bin/python3 -c "from softwareproperties.SoftwareProperties import SoftwareProperties; SoftwareProperties(deb822=True).enable_source_code_sources()" \
		&& $(AptUpdate)

install-nkf:
	which nkf || $(AptInstall) nkf

install-apt-file:
	which apt-file || ( \
		$(AptInstall) apt-file && \
		sudo apt-file update \
	)

install-luarocks:
	which luarocks || brew install luarocks

install-cmake:
	$(AptInstall) cmake  # To build tmux-cpu-mem-load

install-tmux-runtime-deps:
	$(MAKE) install-cmake

install-peco:
	which peco || brew install peco

install-btop:
	which btop || $(AptInstall) btop

# 画面上にキーを表示するツール
install-keycastow:
	powershell.exe -Command "Start-Process choco -ArgumentList 'install keycastow' -Verb RunAs"
