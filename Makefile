MAKEFLAGS := --no-builtin-rules

USER         := xand
HOST         := hackfreeordie.org
PORT         := 22
USER_AT_HOST := $(USER)@$(HOST)

DIR_LOCAL  := www
DIR_SERVER := /var/www

.PHONY: build
build:
	mkdir -p $(DIR_LOCAL)
	./generate.rkt -o $(DIR_LOCAL)

.PHONY: rebuild
rebuild: clean
	$(MAKE) build

.PHONY: clean
clean:
	rm -rf $(DIR_LOCAL)

.PHONY: deploy
deploy:
	rsync \
		-avz \
		--delete \
		--omit-dir-times \
		--copy-links \
		./$(DIR_LOCAL)/* \
		-e 'ssh -p $(PORT)' \
		$(USER_AT_HOST):$(DIR_SERVER)
	ssh -p $(PORT) $(USER_AT_HOST) chmod -R a+rX $(DIR_SERVER)

.PHONY: install_deps
install_deps:
	raco pkg install --skip-installed --auto gregor markdown

.PHONY: TODO
TODO:
	@grep \
		--exclude=Makefile \
		--exclude-dir=$(DIR_LOCAL) \
		--exclude-dir=.git \
		--color=always \
		-rIHn TODO .
