build: src/*.elm
	mkdir -p build
	elm-make src/Main.elm --output build/elm.js

gh-pages: build
	mkdir -p gh-pages
	cp build/elm.js        gh-pages/
	cp static/fonts/*.ttf  gh-pages/
	cp static/html/*.css   gh-pages/
	cp static/html/*.html  gh-pages/
	cp static/images/*.png gh-pages/

android: build
	mkdir -p android/assets
	cp build/* android/assets/
	cp static/fonts/* android/assets
	cp static/html/* android/assets
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A magic

emulate: android
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A emulate-magic
	./result/bin/run-test-emulator

deploy: android
	sudo adb -d uninstall org.nomath.magic
	sudo adb -d install -r result/Magic-debug.apk

clean:
	rm -rf build

dist-clean: clean
	rm -rf elm-stuff
	rm -rf android/assets
	rm -rf gh-pages/*

