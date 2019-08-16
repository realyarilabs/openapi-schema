.PHONY: clean style test coverage

test:
	stack test

coverage:
	stack test --coverage

clean:
	stack clean --full

style:
	git ls-files | grep '\.l\?hs$$' | xargs stylish-haskell -c .stylish-haskell.yaml -i
