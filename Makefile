ifneq (,$(findstring GNU,$(shell cp --version 2>/dev/null || true)))
	# GNU
	cp := cp --reflink=auto
else
	# BSD (including macOS)
	cp := cp -c
endif

build_page := target/debug/build_page

static_files := _site/index.html _site/styles.css

print_status = @ printf ' \033[1;35m♦ %s\033[0m\n' '$(1)'

dates_of_blog_paths = $(foreach date_slug,$(notdir $(basename $1)),$(shell echo $(date_slug) | cut -d- -f-3))
slugs_of_blog_paths = $(foreach date_slug,$(notdir $(basename $1)),$(shell echo $(date_slug) | cut -d- -f4-))
blog_posts := $(foreach slug,$(call slugs_of_blog_paths,$(wildcard blog/*.md)),_site/blog/$(slug)/index.html)

.PHONY: all
all: $(blog_posts) $(static_files)

$(build_page): target/debug/%: $(shell find src) Cargo.toml
	$(call print_status,Cargo $(@F))
	@ cargo build --bin $(@F) --locked

_site/blog/%/index.html: blog/*-%.md $(build_page)
	$(call print_status,Blog $*)
	@ mkdir -p $(dir $@)
	@ $(build_page) $(call dates_of_blog_paths,$<) $< > $@

$(static_files): _site/%: %
	$(call print_status,Copy $*)
	@ mkdir -p $(dir $@)
	@ cp $^ $@

.PHONY: clean
clean:
	$(call print_status,Clean)
	@ cargo clean
	@ rm -fr _site
