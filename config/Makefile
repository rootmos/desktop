CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

define service
	ln -svf "$(CURRENT_DIR)/systemd/$(strip $(1)).service" "$(HOME)/.config/systemd/user/$(strip $(1)).service"
	systemctl --user enable $(strip $(1)).service
	systemctl --user restart $(strip $(1)).service
endef

.PHONY: install
install: statusbar displayswitcheroo

.PHONY: statusbar
statusbar:
	ln -svf "$(CURRENT_DIR)/statusbar" "$(HOME)/.config/statusbar"

.PHONY: displayswitcheroo
displayswitcheroo:
	ln -svf "$(CURRENT_DIR)/displayswitcheroo" "$(HOME)/.config/displayswitcheroo"
	$(call service, displayswitcheroo)
