MAKEFLAGS := --no-builtin-rules

USER         := xand
HOST         := hackfreeordie.org
PORT         := 22
USER_AT_HOST := $(USER)@$(HOST)

.PHONY: generate
generate:
	mkdir -p dist
	./generate.rkt

.PHONY: deploy
deploy:
	rsync \
		-avz \
		--delete \
		--omit-dir-times \
		--copy-links \
		./dist/* \
		-e 'ssh -p $(PORT)' \
		$(USER_AT_HOST):/var/www
	ssh -p $(PORT) $(USER_AT_HOST) chmod -R a+rX /var/www
