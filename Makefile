FLAGS := -j --disable-documentation

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

all:
	make clean; make install && make build

install: cabal.sandbox.config
	cabal install $(FLAGS) \
 --only-dependencies \
 --force-reinstalls

clean:
	cabal clean
	rm -rf cabal.sandbox.config .cabal-sandbox

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init
