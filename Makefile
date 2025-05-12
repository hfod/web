N_CPUS := $(shell nproc 2> /dev/null || gnproc 2> /dev/null || sysctl -n hw.ncpu 2> /dev/null)

MAKEFLAGS := --no-builtin-rules -j $(N_CPUS)

DIR_DATA       := data
DIR_CACHE      := .cache
DIR_ARTIFACTS  := www

.PHONY: build
build: web

.PHONY: web
web:
	mkdir -p $(DIR_ARTIFACTS)
	RUST_BACKTRACE=1 cargo run --release --bin hfod-web-gen -- \
		--in $(DIR_DATA) \
		--out $(DIR_ARTIFACTS) \
		gen \
			--minify

.PHONY: serve
serve:
	RUST_BACKTRACE=1 cargo run --bin hfod-web-srv -- \
		-l debug \
		--addr 127.0.0.1:8080 \
		--web-dir $(DIR_ARTIFACTS)

.PHONY: rebuild
rebuild: clean_artifacts clean_cache
	$(MAKE) build

.PHONY: clean_artifacts
clean_artifacts:
	rm -rf $(DIR_ARTIFACTS)

.PHONY: clean_cache
clean_cache:
	rm -rf $(DIR_CACHE)

.PHONY: preview
preview: rebuild
	$(MAKE) serve

.PHONY: publish
publish:
	RUST_BACKTRACE=1 cargo run --bin hfod-web-pub -- -l debug $(DIR_ARTIFACTS)

.PHONY: TODO
TODO:
	@grep \
		--exclude=Makefile \
		--exclude-dir=view \
		--exclude-dir=$(DIR_ARTIFACTS) \
		--exclude-dir=.git \
		--color=always \
		-rIHn TODO .

.PHONY: strip
strip:
	# Strip metadata from photos:
	find data/meetings -maxdepth 2 -type d -name photos \
	| xargs -I% find % -maxdepth 1 -type f -iname '*.jp*g' \
	| xargs -P $(N_CPUS) -I% sh -c 'exiftool -all= % && rm -f %_original'
