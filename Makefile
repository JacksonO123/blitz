target: .FORCE_DEBUG

prod: .FORCE_PROD

.FORCE_DEBUG:
	zig build -p build

.FORCE_PROD:
	zig build -p build -Doptimize=ReleaseFast --release=fast
