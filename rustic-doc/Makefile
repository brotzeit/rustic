# Used for development
standard:
	cd debug_files ;\
	pandoc enum.Option.html  --lua-filter ../filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o option.org

standard_unfiltered:
	cd debug_files ;\
	pandoc enum.Option.html -t native -o filterednative ;\
	pandoc -f native filterednative -o option.org


primitive:
	cd debug_files ;\
	pandoc primitive.i16.html  --lua-filter ../filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o primitive.i16.org


primitive_unfiltered:
	cd debug_files ;\
	pandoc primitive.i16.html -t native -o filterednative ;\
	pandoc -f native filterednative -o primitive.i16.org

trait:
	cd debug_files ;\
	pandoc trait.AsRef.html --lua-filter ../filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o AsRef.org

code:
	cd debug_files ;\
	pandoc shared.rs.html --lua-filter ../filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o shared.rs.org

constant:
	cd debug_files ;\
	pandoc constant.ARCH.html --lua-filter ../filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o constant.arch.org

overwrite_filter:
	rm ~/.local/bin/rustic-doc-filter.lua
	cp ./filter.lua ~/.local/bin/rustic-doc-filter.lua
