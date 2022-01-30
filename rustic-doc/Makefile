# Used for development
standard: create_filter
	cd debug_files ;\
	pandoc enum.Option.html -t json | ../pandoc_filter/rustdoc-to-org-exe | pandoc -f json -t native -o filterednative ;\
	pandoc -f native filterednative -o option.org

trait: create_filter
	cd debug_files ;\
	pandoc trait.AsRef.html -t json | ../pandoc_filter/rustdoc-to-org-exe | pandoc -f json -t native -o filterednative ;\
	pandoc -f native filterednative -o AsRef.org

code: create_filter
	cd debug_files ;\
	pandoc shared.rs.html -t json | ../pandoc_filter/rustdoc-to-org-exe | pandoc -f json -t native -o filterednative ;\
	pandoc -f native filterednative -o shared.rs.org

constant: create_filter
	cd debug_files ;\
	pandoc constant.ARCH.html -t json | ../pandoc_filter/rustdoc-to-org-exe | pandoc -f json -t native -o filterednative ;\
	pandoc -f native filterednative -o constant.arch.org

create_filter:
	cd pandoc_filter; \
	stack install --local-bin-path .; \
	cd ..

overwrite_local:
	cd pandoc_filter; \
	stack install --local-bin-path ~/.local/bin/rustic-doc-filter; \
	cd ..
	cp convert.sh ~/.local/bin/rustic-doc-convert.sh
