.PHONY: install
install:
	git submodule update --init
	make -C xmonad install
	make -C st install DESTDIR="$(HOME)/.local" PREFIX=
	make -C dmenu install DESTDIR="$(HOME)/.local" PREFIX= MANPREFIX=/share/man
	make -C statusbar install
	make -C dvorak install
	make -C xhook install
	make -C action/c-impl install
	make -C displayswitcheroo install
	make -C monitor install SUDO=sudo

.PHONY: config
config:
	make -C config install

.PHONY: deps
deps:
	pacman -S --needed --noconfirm \
		libx11 libxss libxft libxrandr \
		python-pipx \
		dzen2 conky \
		ttf-cascadia-code \
		xorg-xkbcomp xorg-setxkbmap \
		opam socat
