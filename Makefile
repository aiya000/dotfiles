all: install

OS ?= WSL

YayInstall = yay -S --needed --noconfirm
YayUpdate = yay -Sy

AptInstall = sudo apt-fast install -y
AptUpdate = sudo apt-fast update
AptBuildDep = sudo apt-fast build-dep

BrewInstall = brew install

NPMInstall = npm install --global --user
LuaRocksInstall = luarocks install --local --lua-version=5.1 # 5.1 because Neovim's Lua is 5.1 or LuaJit
UVInstall = uv tool install # 事前に`load-my-env mise`してね。まだmiseでuvを入れてなければ`mise use uv@latest`もしよう！
PIPInstall = $(UVInstall) # pip installよくわからん
MiseUse = mise use -g

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
	# Please see .vimrc and .shells
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

install-ghcup:
	which ghcup || curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

install-dotnet:
	which dotnet || $(MiseUse) dotnet@9

install-dotnet-script:
	which dotnet-script || dotnet tool install -g dotnet-script

install-scala:
	which 
	mise use -g scala@3

install-idris2:
	which idris2 || $(BrewInstall) idris2

install-koka:
	which koka || $(BrewInstall) koka

network-config:
	sudo systemctl enable dhcpcd
	sudo systemctl enable NetworkManager

install-bun:
	which bun || $(BrewInstall) oven-sh/bun/bun

install-pnpm:
	which pnpm || $(NPMInstall) pnpm@latest-10

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
	which luap || $(LuaRocksInstall) luaprompt

install-editprompt:
	which editprompt || $(NPMInstall) editprompt


install-ttyd:
	which ttyd || $(BrewInstall) ttyd

install-vhs:
	$(MAKE) install-ttyd
	which vhs || $(BrewInstall) vhs


install-ffmpeg:
	which ffmpeg || $(BrewInstall) ffmpeg

install-ccstatusline:
	which ccstatusline || $(NPMInstall) ccstatusline@latest

install-dust:
	which dust || $(BrewInstall) dust

install-android-cmdline-tools:
	[[ -d ~/android-sdk/cmdline-tools ]] || ( \
		cd /tmp && \
		wget https://dl.google.com/android/repository/commandlinetools-mac-14742923_latest.zip && \
		mkdir -p ~/android-sdk/cmdline-tools && \
		unzip commandlinetools-mac-14742923_latest.zip -d ~/android-sdk/cmdline-tools && \
		mv ~/android-sdk/cmdline-tools/cmdline-tools ~/android-sdk/cmdline-tools/latest && \
		rm /tmp/commandlinetools-mac-14742923_latest.zip && \
	: )

# https://cli.devinenterprise.com/docs
install-devin-cli:
	which devin || curl -fsSL https://cli.devin.ai/install.sh | bash

# }}}
ifeq ($(OS),WSL) # {{{

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

# https://github.com/Xfennec/progress
install-progress:
	which progress || $(AptInstall) progress

install-fzf:
	which fzf || $(BrewInstall) fzf

install-android-cmdline-tools:
	[[ -d ~/Android/Sdk/cmdline-tools ]] || \
	cd /tmp && \
	wget https://dl.google.com/android/repository/commandlinetools-linux-11076708_latest.zip && \
	mkdir -p Android/Sdk/cmdline-tools && \
	unzip commandlinetools-linux-11076708_latest.zip -d Android/Sdk/cmdline-tools && \
	mv Android/Sdk/cmdline-tools/cmdline-tools Android/Sdk/cmdline-tools/latest && \
	rm commandlinetools-linux-11076708_latest.zip && \
	mv Android ~ && \
	:

endif # }}}
ifneq ($(filter $(OS),Darwin macos),) # {{{

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

install-ghostscript:
	ls /opt/homebrew/Cellar/ghostscript &> /dev/null || $(BrewInstall) ghostscript

install-imagemagick:
	which magick || $(BrewInstall) imagemagick

install-shellcheck:
	which shellcheck || $(BrewInstall) shellcheck

install-jq:
	which jq || $(BrewInstall) jq

# Required by Raycast OCR extension (Easy OCR)
install-tesseract:
	which tesseract || $(BrewInstall) tesseract
	if [[ $(tesseract --list-langs | rg jpn | wc -l | sed 's/ //g') == 0 ]] ; then \
		$(BrewInstall) tesseract-lang ; \
	fi

# Required by notify-at and notify-cascade
install-alerter:
	cd ~/bin && \
	wget https://github.com/vjeantet/alerter/releases/download/1.0.0/alerter_v1.0.0_darwin_amd64.zip && \
	unzip alerter_v1.0.0_darwin_amd64.zip && \
	which alerter && \
	rm-dust alerter_v1.0.0_darwin_amd64.zip

install-grip:
	which grip || $(BrewInstall) grip

install-p7zip:
	which 7z || $(BrewInstall) p7zip

install-atool:
	which aunpack || $(BrewInstall) atool

install-atool-deps:
	$(MAKE) install-p7zip

install-mysql-client:
	which mysql || $(BrewInstall) mysql-client

install-git-lfs:
	which git-lfs || $(BrewInstall) git-lfs

install-coreutils: install-gnu-ls
install-gls: install-gnu-ls
install-gnu-ls:
	which gls || $(BrewInstall) coreutils

install-brew:
	which brew || \
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" && \
		exec zsh

install-tree-sitter:
	brew info tree-sitte > /dev/null 2>&1 || $(BrewInstall) tree-sitter

# https://junegunn.github.io/fzf/installation/
install-fzf:
	which fzf || $(BrewInstall) fzf

install-cocoapods:
	brew info cocoapods > /dev/null 2>&1 || $(BrewInstall) cocoapods

# https://github.com/asmvik/skhd
install-skhd:
	which skhd || \
  		$(BrewInstall) asmvik/formulae/skhd && \
  		skhd --start-service && \
		:

install-gsed: install-gnu-sed
install-gnu-sed:
	which gsed || $(BrewInstall) gnu-sed

endif # }}}

download-nerd-fonts:
	cd ~/Downloads ; \
		wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Hack.zip ; \
		explorer.exe .
	@echo 'Install HackNerdFontMono-Regular.ttf, and set it to \'プロファイル > Ubuntu > 外観 > フォントフェイス\' manually'

open-ditt-page:
	xdg-open https://sabrogden.github.io/Ditto/
	@echo 'Next, to enable paste to Windows Terminal by Insert+Shift:'
	@echo '1. Download **Portable** version Ditto'
	@echo '2. Extract zip'
	@echo '3. Move Ditto folder to "C:\Users\UserName\Program Files\Ditto" using explorer.exe (explorer.exe because this needs Windows admin permission)'
	@echo '4. Start Ditto.exe once'
	@echo '5. Open Ditto options from system tray icon'
	@echo '6. Change some settings in Ditto to generate "C:\Users\UserName\Program Files\Ditto\Ditto.settings"'
	@echo '7. Close (Kill) Ditto from system tray'
	@echo '8. Add "DefaultPasteString=+{INS}" to [Ditto] section in Ditto.settings - **NOTE:** This .settings is easy to read. Please care to paste into [Ditto]'
	@echo '9. Start Ditto.exe again'

# vim:foldmethod=marker
