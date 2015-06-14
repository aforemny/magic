gh-pages: Main.elm
	mkdir -p gh-pages
	elm-make Main.elm
	cp index.html gh-pages/
	cp elm.js gh-pages/html.js
