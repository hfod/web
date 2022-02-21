N_CPUS := $(shell nproc 2> /dev/null || gnproc 2> /dev/null || sysctl -n hw.ncpu 2> /dev/null)

MAKEFLAGS := --no-builtin-rules -j $(N_CPUS)

USER         := xand
HOST         := hackfreeordie.org
PORT         := 22
USER_AT_HOST := $(USER)@$(HOST)

DIR_EMAIL      := email
DIR_WEB_LOCAL  := www
DIR_WEB_SERVER := /var/www

CMD_GENERATE := ./generate

.PHONY: build
build: web email

.PHONY: web
web:
	mkdir -p $(DIR_WEB_LOCAL)
	$(CMD_GENERATE) -o $(DIR_WEB_LOCAL) web

.PHONY: email
email:
	mkdir -p $(DIR_EMAIL)
	$(CMD_GENERATE) -o $(DIR_EMAIL) email

.PHONY: rebuild
rebuild: clean
	$(MAKE) build

.PHONY: clean
clean:
	rm -rf $(DIR_WEB_LOCAL) $(DIR_EMAIL)

.PHONY: deploy
deploy:
	rsync \
		-avz \
		--delete \
		--omit-dir-times \
		--copy-links \
		./$(DIR_WEB_LOCAL)/* \
		-e 'ssh -p $(PORT)' \
		$(USER_AT_HOST):$(DIR_WEB_SERVER)
	ssh -p $(PORT) $(USER_AT_HOST) chmod -R a+rX $(DIR_WEB_SERVER)

.PHONY: install_deps
install_deps:
	raco pkg install --skip-installed --auto gregor markdown

.PHONY: TODO
TODO:
	@grep \
		--exclude=Makefile \
		--exclude-dir=$(DIR_WEB_LOCAL) \
		--exclude-dir=.git \
		--color=always \
		-rIHn TODO .
