build: 
	nix-build -A nix-mate.components.exes.nix-mate --quiet

run:
	make build
	./result/bin/nix-mate

build-test:
	nix-build -A nix-mate.components.tests.nix-mate-test --quiet

unit-test:
	make build-test
	./result/bin/nix-mate-test
