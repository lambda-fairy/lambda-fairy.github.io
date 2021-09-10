build_blog_manifest := target/debug/build_blog_manifest
build_blog_post := target/debug/build_blog_post

blog_manifest := _site/index.html

dates_of_blog_paths = $(foreach date_slug,$(notdir $(basename $1)),$(shell echo $(date_slug) | cut -d- -f-3))
slugs_of_blog_paths = $(foreach date_slug,$(notdir $(basename $1)),$(shell echo $(date_slug) | cut -d- -f4-))
blog_sources := $(wildcard blog/*.md)
published_blog_sources := $(filter-out blog/0-draft-0-%,$(blog_sources))
blog_posts := $(foreach slug,$(call slugs_of_blog_paths,$(blog_sources)),_site/blog/$(slug)/index.html)

static_files := $(addprefix _site/,styles.css $(shell find images -type f))

print_status = @ printf ' \033[1;35m♦ %s\033[0m\n' '$(1)'

.PHONY: all
all: $(blog_manifest) $(blog_posts) $(static_files)

$(build_blog_manifest) $(build_blog_post): target/debug/%: $(shell find src) Cargo.toml
	$(call print_status,Cargo $(@F))
	@ cargo build --bin $(@F) --locked

$(blog_manifest): $(published_blog_sources) $(build_blog_manifest)
	$(call print_status,Manifest)
	@ mkdir -p $(dir $@)
	@ $(build_blog_manifest) \
		$(foreach blog_path,$(published_blog_sources), \
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
