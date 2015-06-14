build: Main.elm
	elm-make Main.elm

gh-pages: build
	mkdir -p gh-pages
	cp index.html gh-pages/
	cp elm.js gh-pages/
