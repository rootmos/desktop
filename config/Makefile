CURRENT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

define service
	ln -svf "$(CURRENT_DIR)/systemd/$(strip $(1)).service" "$(HOME)/.config/systemd/user/$(strip $(1)).service"
	systemctl --user enable $(strip $(1)).service
	systemctl --user restart $(strip $(1)).service
endef

.PHONY: install
install: statusbar displayswitcheroo \
	monitor.service bluetooth-codec-fix.service

.PHONY: statusbar
statusbar:
	ln -svf "$(CURRENT_DIR)/statusbar" "$(HOME)/.config/statusbar"

.PHONY: displayswitcheroo
displayswitcheroo: displayswitcheroo.service
	ln -svf "$(CURRENT_DIR)/displayswitcheroo" "$(HOME)/.config/displayswitcheroo"

%.service:
	$(call service, $*)
