build: src/*.elm
	mkdir -p build
	elm-make src/Main.elm --output build/elm.js

dist-web: build
	mkdir -p gh-pages
	cp build/elm.js         gh-pages/
	cp static/fonts/*.ttf   gh-pages/
	cp static/html/*.css    gh-pages/
	cp static/html/*.html   gh-pages/

distribute: dist-web dist-firefox-os dist-android

build-android: build
	mkdir -p android/assets
	cp build/* android/assets/
	cp static/fonts/* android/assets
	cp static/html/* android/assets
	sed -i 's/animation/-webkit-animation/g' android/assets/*.css
	sed -i 's/keyframes/-webkit-keyframes/g' android/assets/*.css
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A magic

emulate: build-android
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A emulate-magic
	./result/bin/run-test-emulator

deploy: build-android
	sudo adb -d uninstall org.nomath.magic
	sudo adb -d install -r result/Magic-debug.apk

dist-android: dist-web build-android
	mkdir -p gh-pages/get-android
	cp -f result/Magic-debug.apk gh-pages/get-android/magic.apk

dist-firefox-os: dist-web
	zip -j build/package.zip static/html/*.html build/*.js static/html/*.css resources/*.png static/images/*.png static/fonts/*.ttf firefox-os/manifest.webapp build/package/
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

