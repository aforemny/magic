build: Main.elm
	elm-make Main.elm

gh-pages: build
	mkdir -p gh-pages
	cp index.html gh-pages/
	cp site.css gh-pages/
	cp elm.js gh-pages/
	cp *.png gh-pages
