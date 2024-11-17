.PHONY: install
install:
	git submodule update --init
	make -C xmonad install
	make -C st install DESTDIR="$(HOME)/.local" PREFIX=
	make -C statusbar install
	make -C dvorak install

.PHONY: deps
deps:
	pacman -S --needed --noconfirm \
		libx11 libxss libxft libxrandr \
		python-pipx \
		dzen2 conky dmenu \
		ttf-cascadia-code \
		xorg-xkbcomp xorg-setxkbmap
