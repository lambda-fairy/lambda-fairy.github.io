ifneq (,$(findstring GNU,$(shell cp --version 2>/dev/null || true)))
	# GNU
	cp := cp --reflink=auto
else
	# BSD (including macOS)
	cp := cp -c
endif

build_blog_manifest := target/debug/build_blog_manifest
build_blog_post := target/debug/build_blog_post

blog_manifest := _site/blog/index.html

dates_of_blog_paths = $(foreach date_slug,$(notdir $(basename $1)),$(shell echo $(date_slug) | cut -d- -f-3))
slugs_of_blog_paths = $(foreach date_slug,$(notdir $(basename $1)),$(shell echo $(date_slug) | cut -d- -f4-))
blog_posts := $(foreach slug,$(call slugs_of_blog_paths,$(wildcard blog/*.md)),_site/blog/$(slug)/index.html)

static_files := $(addprefix _site/,index.html styles.css $(shell find images -type f))

print_status = @ printf ' \033[1;35m♦ %s\033[0m\n' '$(1)'

.PHONY: all
all: $(blog_manifest) $(blog_posts) $(static_files)

$(build_blog_manifest) $(build_blog_post): target/debug/%: $(shell find src) Cargo.toml
	$(call print_status,Cargo $(@F))
	@ cargo build --bin $(@F) --locked

$(blog_manifest): blog/*.md $(build_blog_manifest)
	$(call print_status,Manifest)
	@ mkdir -p $(dir $@)
	@ $(build_blog_manifest) \
		$(foreach blog_path,$(wildcard blog/*.md), \
			$(call dates_of_blog_paths,$(blog_path)):$(call slugs_of_blog_paths,$(blog_path)):$(blog_path)) \
		> $@

_site/blog/%/index.html: blog/*-%.md $(build_blog_post)
	$(call print_status,Blog $*)
	@ mkdir -p $(dir $@)
	@ $(build_blog_post) $(call dates_of_blog_paths,$<) $< > $@

$(static_files): _site/%: %
	$(call print_status,Copy $*)
	@ mkdir -p $(dir $@)
	@ cp $^ $@

.PHONY: clean
clean:
	$(call print_status,Clean)
	@ cargo clean
	@ rm -fr _site
