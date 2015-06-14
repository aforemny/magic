build: *.elm
	elm-make TwoPlayer.elm --output twoplayer.js
	elm-make TwoPlayerPrime.elm --output twoplayerprime.js
	elm-make ThreePlayer.elm --output threeplayer.js
	elm-make FourPlayer.elm --output fourplayer.js
	elm-make FivePlayer.elm --output fiveplayer.js

gh-pages: build
	mkdir -p gh-pages
	cp *.html gh-pages/
	cp *.js gh-pages/
	cp *.css gh-pages/
	cp *.png gh-pages
	cp twoplayer.html gh-pages/index.html

clean:
	rm *.js
