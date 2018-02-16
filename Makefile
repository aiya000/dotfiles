all: install

prepare:
	[ ! -d ~/bin ] && mkdir ~/bin
	[ ! -d ~/git ] && mkdir ~/git

#TODO: Install stack, npm, and pip at first
# Install outer dependencies
install:
	make prepare
	make install_vim_deps

install_vim_deps:
	sudo npm install -g doctoc shiba textlint
	sudo -H pip install grip
	git clone https://github.com/jszakmeister/markdown2ctags ~/git/markdown2ctags
	ln -s ~/git/markdown2ctags/markdown2ctags.py ~/bin
	stack install hasktags haskdogs
