build: *.elm
	elm-make Portrait2.elm  --output portrait2.js
	#elm-make ThreePlayer.elm --output threeplayer.js
	#elm-make FourPlayer.elm --output fourplayer.js
	#elm-make FivePlayer.elm --output fiveplayer.js

gh-pages: build
	mkdir -p gh-pages
	cp *.js gh-pages/
	cp *.ttf gh-pages/
	cp settings.png gh-pages/
	cp site.css gh-pages/
	cp portrait2.css gh-pages/
	cp portrait2.html gh-pages/index.html
	cp package.zip gh-pages/
	cp mini-manifest.webapp gh-pages/
	cp install.html gh-pages/


android: build
	mkdir -p android/assets
	cp *.html android/assets/
	cp *.js   android/assets/
	cp *.css  android/assets/
	cp *.png  android/assets/
	cp *.ttf  android/assets/
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A magic

firefox-os: build
	zip package.zip *.html *.js *.css img/*.png *.png *.ttf manifest.webapp

emulate: android
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A emulate-magic
	./result/bin/run-test-emulator

deploy: android
	sudo adb -d uninstall org.nomath.magic
	sudo adb -d install -r result/Magic-debug.apk

clean:
	rm -f *.js

dist-clean: clean
	rm -rf elm-stuff
	rm -rf android/assets
	rm -rf gh-pages/*

