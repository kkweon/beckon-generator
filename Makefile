all:
	echo "Hi"

format:
	fd ".hs" | xargs -n1 brittany --write-mode inplace

lint:
	hlint app src
