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
	yaourt -S git neovim
	yaourt -S --noconfirm z3  # Needed by liquidhaskell
	git clone https://github.com/jszakmeister/markdown2ctags ~/git/markdown2ctags
	ln -s ~/git/markdown2ctags/markdown2ctags.py ~/bin

install_with_stack:
	git clone --recursive https://github.com/ucsd-progsys/liquidhaskell ~/git/liquidhaskell
	cd ~/git/liquidhaskell
	stack install

install_with_npm:
	sudo -H npm install -g doctoc shiba textlint

install_with_pip:
	sudo -H pip install neovim grip