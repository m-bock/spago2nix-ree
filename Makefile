psa := psa \
		--is-lib=.spago \
		--stash \
		--censor-lib \
		--strict \
		'.spago/*/*/src/**/*.purs'

build:
	$(psa) 'src/**/*.purs' 'test/**/*.purs'
	parcel build --target node --no-source-maps  src/index.js .
	mkdir -p dist/bin
	echo "#!/usr/bin/env node" > dist/bin/spago2nix-ree
	cat dist/index.js >> dist/bin/spago2nix-ree
	chmod +x dist/bin/spago2nix-ree
	rm dist/index.js

check:
	node -e "require('./output/Test.Main').main()"

installcheck:
	$$out/bin/spago2nix-ree --help

version:
	sed -i 's/$(OLD)/$(NEW)/g' README.md
	sed -i 's/$(OLD)/$(NEW)/g' spago2nix-ree-cli.nix
	sed -i 's/$(OLD)/$(NEW)/g' package.json
	git tag $(NEW)