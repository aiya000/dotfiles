all: install

prepare:
	if [ ! -d ~/bin ] ; then\
		mkdir ~/bin ;\
	fi
	if [ ! -d ~/git ] ; then\
		mkdir ~/git ;\
	fi

#TODO: Don't depend arch linux
# Install outer dependencies
install:
	make prepare
	make install_package_managers
	make install_general_stuff
	make install_with_stack
	make install_with_npm
	make install_with_pip

	stack install hasktags haskdogs

install_package_managers:
	yaourt -Sy
	yaourt -S --noconfirm static-stack npm python-pip

install_general_stuff:
	yaourt -S --noconfirm \
		git neovim tmux autoreconf \
		z3  # Needed by liquidhaskell
		haskell-ide-engine # by LanguageClient-neovim
	brew install --with-clang --with-lld --with-python --HEAD llvm cppunit  # vim-textobj-clang
	git clone https://github.com/jszakmeister/markdown2ctags ~/git/markdown2ctags
	ln -s ~/git/markdown2ctags/markdown2ctags.py ~/bin

install_with_stack:
	stack install ghcid # .vimrc
	git clone --recursive https://github.com/ucsd-progsys/liquidhaskell ~/git/liquidhaskell
	cd ~/git/liquidhaskell
	stack install

install_with_npm:
	sudo -H npm install -g doctoc shiba textlint

install_with_pip:
	sudo -H pip install neovim grip

install_if_linux:
	#TODO: Return at here if the OS is not linux
	:
	yaourt -S --noconfirm dzen2 # xmonad
	yaourt -S --noconfirm rxvt-unicode # xmonad
	yaourt -S --nocnfirm sox # say-result
	# Fix East Asian Ambiguous character width problems
	git clone https://github.com/fumiyas/wcwidth-cjk ~/git/wcwidth-cjk
	pushd .
	cd ~/git/wcwidth-cjk
	autoreconf --install
	./configure --prefix=/usr/local/
	make
	sudo make install
	popd
