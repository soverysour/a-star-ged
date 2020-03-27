default: run

interactive: lint-format ghcid

ghcid:
	-@clear
	-@ghcid --command 'stack ghci'

lint-format:
	-@hlint app/
	-@hlint src/
	-@hlint test/
	-@find app/ -name '*.hs' | xargs hindent
	-@find src/ -name '*.hs' | xargs hindent
	-@find test/ -name '*.hs' | xargs hindent
	-@find app/ -name '*.hs' | xargs stylish-haskell -i
	-@find src/ -name '*.hs' | xargs stylish-haskell -i
	-@find test/ -name '*.hs' | xargs stylish-haskell -i

profile:
	-@clear
	-@stack build --profile
	-@stack exec --profile -- ged-seq-exe +RTS -p -hc -i0.01
	-@stack exec -- hp2ps -c ged-seq-exe.hp && ps2pdf ged-seq-exe.ps
	-@mv ged-seq-exe.prof profiling-data/prof/`date -Iminutes`.prof
	-@mv ged-seq-exe.hp profiling-data/heap-prof/`date -Iminutes`.hp
	-@mv ged-seq-exe.pdf profiling-data/heap-vis/`date -Iminutes`.pdf
	-@profiteur profiling-data/prof/`date -Iminutes`.prof
	-@mv profiling-data/prof/`date -Iminutes`.prof.html profiling-data/prof-vis/`date -Iminutes`.prof.html
	-@rm ged-seq-exe.aux
	-@rm ged-seq-exe.ps

setup-profile:
	-@mkdir -p profiling-data/heap-prof
	-@mkdir -p profiling-data/heap-vis
	-@mkdir -p profiling-data/prof
	-@mkdir -p profiling-data/prof-vis

clean-profile:
	-@rm profiling-data/heap-prof/*
	-@rm profiling-data/heap-vis/*
	-@rm profiling-data/prof/*
	-@rm profiling-data/prof-vis/*

run:
	-@stack run ged-seq-exe

trace:
	-@stack test --trace --coverage

test:
	-@stack test --coverage

doc:
	-@stack haddock

clean:
	-@stack clean
	-@clear

.PHONY: default interactive ghcid lint-format profile setup-profile clean-profile run trace test doc clean
