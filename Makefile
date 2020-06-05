all: dist

main.js: src/Main.elm
	elm make src/Main.elm --output main.js

.PHONY: dist
dist: main.js index.html
	rm -r target || true
	mkdir target
	cp -r js target
	cp -r assets target
	cp main.js target/main-$(shell md5sum main.js | cut -d " " -f 1).js
	cat index.html | sed s/main.js/main-$(shell md5sum main.js | cut -d " " -f 1).js/ > target/index.html

clean:
	rm main.js || true
	rm -r target || true
