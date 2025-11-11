all: install

logfile = ./dotfiles-MakeFile.log

# TODO: Detect auto
OS ?= Ubuntu
WSL2 ?= yes # 'no' or 'yes'

YayInstall = yay -S --needed --noconfirm
YayUpdate = yay -Sy

AptInstall = sudo apt-fast install -y
AptUpdate = sudo apt-fast update
AptBuildDep = sudo apt-fast build-dep

BrewInstall = brew install

NPMInstall = npm install --global --user
UVInstall = uv tool install # 事前に`load-my-env mise`してね。まだmiseでuvを入れてなければ`mise use uv@latest`もしよう！
PIPInstall = $(UVInstall) # pip installよくわからん

# Common {{{

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
ifeq ($(WSL2),yes)
	WinUserName := $(/mnt/c/Windows/system32/whoami.exe | tr -d '\r\n' | cut -d '\' -f 2)
	ln -s /mnt/c/Users/$(WinUserName) ~/Windows
	ln -s /mnt/c/Users/$(WinUserName)/Desktop ~/Desktop
	ln -s /mnt/c/Users/$(WinUserName)/Download ~/Download
endif

install:
	$(MAKE) prepare
	$(MAKE) install-core-package-managers
	$(MAKE) build-os-env

install-neovim:
	which nvim || $(BrewInstall) neovim

install-nvm:
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.2/install.sh | bash

install-haskell-stack:
	curl -sSL https://get.haskellstack.org/ | sh

network-config:
	sudo systemctl enable dhcpcd
	sudo systemctl enable NetworkManager

install-bun:
	which bun || $(BrewInstall) oven-sh/bun/bun

install-deno:
	which deno || $(BrewInstall) deno

install-doctoc:
	which doctoc || $(NPMInstall) doctoc

install-glow:
	which glow || $(BrewInstall) glow

install-text:
	which textlint || $(NPMInstall) textlint

install-typescript:
	which typescript || $(NPMInstall) typescript

install-ts-node:
	which ts-node || $(NPMInstall) ts-node

install-tsx:
	which tsx || $(NPMInstall) tsx

install-html:
	which htmlhint || $(NPMInstall) htmlhint

install-css:
	which csslint || $(NPMInstall) csslint

install-xml:
	which xml || $(NPMInstall) pretty-xml

install-vim-runtime-deps:
	$(MAKE) install-deno
	$(MAKE) install-gtran
	$(MAKE) install-xclip
	$(MAKE) install-pup
	$(MAKE) install-vital-vim

install-pup:
	which pup || go install github.com/ericchiang/pup@latest

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

install-neovim-runtime-deps:
	$(MAKE) install-deno
	$(MAKE) install-gtran
	$(MAKE) install-rg
	$(MAKE) install-nkf

install-lice:
	which lice || $(NPMInstall) lice

install-pandoc:
	which pandoc || $(BrewInstall) pandoc

# See: https://www.rust-lang.org/tools/install
install-cargo:
	which cargo || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

install-stylua:
	which stylua || cargo install stylua

install-claude-code:
	which claude || $(NPMInstall) @anthropic-ai/claude-code

install-mise:
	which mise || curl https://mise.run | sh

install-gemini-cli:
	which gemini || $(NPMInstall) @google/gemini-cli

install-copilot-cli:
	which copilot || $(NPMInstall) @github/copilot

install-luarocks:
	which luarocks || $(BrewInstall) luarocks

install-peco:
	which peco || $(BrewInstall) peco

install-uv:
	which uv || mise use uv@latest

install-luaprompt:
	which luap || luarocks install --local luaprompt

install-editprompt:
	which editprompt || $(NPMInstall) editprompt

# }}}
ifeq ($(OS),Arch) # {{{

# Package managers that core packages depends.
install-core-package-managers:
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

# Install better GUI/CLI environment
build-os-env:
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

install-shellcheck:
	$(YayInstall) shellcheck

install-ruby:
	$(YayInstall) ruby ruby-irb

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

install-java:
	which java || $(YayInstall) jdk

install-displaylink:
	$(YayInstall) displaylink evdi-git

install-recorders:
	$(YayInstall) peek screenkey

install-bluetooth:
	$(YayInstall) bluez bluez-utils bluez-libs bluez-firmware pulseaudio-bluetooth
	sudo modprobe btusb
	# sudo systemctl enable bluetooth
	# sudo systemctl start bluetooth

install-graphic-editors:
	which draw.io || $(YayInstall) drawio-desktop-bin
	which gm || $(YayInstall) graphicsmagick
	which krita || $(YayInstall) krita-appimage

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

endif # }}}
ifeq ($(OS),Ubuntu) # {{{

install-core-package-managers:
	$(MAKE) install-golang
	$(MAKE) install-python3

install-apt-fast:
	sudo add-apt-repository ppa:apt-fast/stable
	sudo apt update
	sudo apt -y install apt-fast

install-fcitx-imlist:
	git clone https://github.com/kenhys/fcitx-imlist /tmp/fcitx-imlist && \
	cd /tmp/fcitx-imlist && \
	./autogen.sh && \
	./configure && \
	make && \
	sudo make install

build-os-env:
	$(AptInstall) \
		git \
		man-db \
		progress \
		rsync \
		tmux \
		vim \
		zsh

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

install-shellcheck:
	$(AptInstall) shellcheck

install-rg:
	which rg || $(AptInstall) ripgrep

install-fd:
	which fd || $(AptInstall) fd-find

# sed alternative
install-sd:
	which sd || cargo install sd

install-nkf:
	which nkf || $(AptInstall) nkf

install-w3m:
	$(AptInstall) w3m

install-brew:
	$(AptInstall) build-essential procps file git
	bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

install-cli-recommended:
	$(BrewInstall) \
		gh \
		watchexec
	$(AptInstall) \
		jq \
		universal-ctags \
		bat

# TODO: Check this make to able to build vim
install-vim-build-deps:
	$(AptBuildDep) vim

install-python3:
	$(AptInstall) python3 python3-pip python-is-python3 python3-venv

install-graphic-editors:
	which drawio || $(NPMInstall) drawio
	which gm || $(AptInstall) graphicsmagick
	which krita || $(AptInstall) krita
	# TODO: Other packages

install-golang:
	sudo add-apt-repository ppa:longsleep/golang-backports
	$(AptUpdate)
	$(AptInstall) golang-go

install-cmake:
	$(AptInstall) cmake  # To build tmux-cpu-mem-load

install-btop:
	which btop || $(AptInstall) btop

install-imagemagick:
	which convert || $(AptInstall) imagemagick

install-ffmpeg:
	which ffmpeg || $(AptInstall) ffmpeg

endif # }}}
ifeq ($(OS),Darwin) # {{{

build-os-env:
	 $(BrewInstall) \
		font-forge \ # for making nerd-fonts for vim-devicons
		cmigemo \ # vim-migemo
		scalastyle \ # ale (vim)
		graphviz plantuml \
		jq \

install-fd:
	which fd || $(BrewInstall) fd

install-rg:
	which rg || $(BrewInstall) ripgrep

endif # }}}
ifeq ($(WSL2),yes) # {{{

install-wslu:
	sudo add-apt-repository ppa:wslutilities/wslu
	$(AptUpdate)
	$(AptInstall) wslu

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

install-sumatora:
	@echo 'ここからインストールしる'
	xdg-open https://www.sumatrapdfreader.org/free-pdf-reader

install-systemctl-for-wsl:
	$(AptInstall) systemd systemd-sysv

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

install-apt-file:
	which apt-file || ( \
		$(AptInstall) apt-file && \
		sudo apt-file update \
	)

install-tmux-runtime-deps:
	$(MAKE) install-cmake

# 画面上にキーを表示するツール
install-keycastow:
	powershell.exe -Command "Start-Process choco -ArgumentList 'install keycastow' -Verb RunAs"

install-win32yank:
	which win32yank.exe || ( \
		cd /tmp && \
		wget https://github.com/equalsraf/win32yank/releases/download/v0.1.1/win32yank-x64.zip && \
		unzip -p win32yank-x64.zip win32yank.exe > ~/bin/win32yank && \
		chmod +x ~/bin/win32yank.exe \
	)

install-google-chrome:
	which google-chrome || ( \
		cd /tmp && \
		wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
		$(AptInstall) ./google-chrome-stable_current_amd64.deb \
	)

endif # }}}

download-nerd-fonts:
	cd ~/Downloads ; \
		wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Hack.zip ; \
		explorer.exe .
	@echo 'Install HackNerdFontMono-Regular.ttf, and set it to \'プロファイル > Ubuntu > 外観 > フォントフェイス\' manually'
