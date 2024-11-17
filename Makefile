.PHONY: install
install:
	git submodule update --init
	make -C xmonad install
	make -C st install DESTDIR="$(HOME)/.local" PREFIX=
	make -C statusbar install

.PHONY: deps
deps:
	pacman -S --needed --noconfirm \
		libx11 libxss libxft \
		python-pipx
