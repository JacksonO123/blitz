target:
	echo "specify blitz file"

%.blitz:
	zig run src/main.zig -- "input/$@"
