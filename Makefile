build: src/*.elm
	mkdir -p build
	elm-make src/Main.elm --output build/elm.js

dist-web: build
	mkdir -p gh-pages
	cp build/elm.js         gh-pages/
	cp static/fonts/*.ttf   gh-pages/
	cp static/html/*.css    gh-pages/
	cp static/html/*.html   gh-pages/
	cp static/images/*.png  gh-pages/

distribute: dist-web dist-firefox-os dist-android

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

dist-android: dist-web android
	mkdir -p gh-pages/get-android
	cp -f result/Magic-debug.apk gh-pages/get-android/magic.apk

firefox-os: build
	zip build/package.zip static/html/*.html build/*.js static/html/*.css resources/*.png static/images/*.png static/fonts/*.ttf firefox-os/manifest.webapp

dist-firefox-os: dist-web firefox-os
	mkdir -p gh-pages/get-firefox-os
	cp build/package.zip               gh-pages/get-firefox-os
	cp firefox-os/mini-manifest.webapp gh-pages/get-firefox-os
	cp firefox-os/install.html         gh-pages/get-firefox-os/index.html

clean:
	rm -rf build

dist-clean: clean
	rm -rf elm-stuff
	rm -rf android/assets
	rm -rf gh-pages/*

