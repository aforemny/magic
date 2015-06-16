build: *.elm
	elm-make Landscape2.elm --output landscape2.js
	elm-make Portrait2.elm  --output portrait2.js
	#elm-make ThreePlayer.elm --output threeplayer.js
	#elm-make FourPlayer.elm --output fourplayer.js
	#elm-make FivePlayer.elm --output fiveplayer.js

gh-pages: build
	mkdir -p gh-pages
	cp *.html gh-pages/
	cp *.js gh-pages/
	cp *.css gh-pages/
	cp *.png gh-pages/
	cp *.ttf gh-pages/
	cp twoplayer.html gh-pages/index.html

android: build
	mkdir -p android/assets
	cp *.html android/assets/
	cp *.js   android/assets/
	cp *.css  android/assets/
	cp *.png  android/assets/
	cp *.ttf  android/assets/
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A magic

emulate: android
	NIX_PATH=nixpkgs=../nixpkgs nix-build default.nix -A emulate-magic
	./result/bin/run-test-emulator

deploy: android
	sudo adb -d uninstall org.nomath.magic
	sudo adb -d install -r result/Magic-debug.apk

clean:
	rm -f *.js
	rm -rf elm-stuff
	rm -rf android/assets

