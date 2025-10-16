target: .FORCE_DEBUG

TEST_CMD=./test

test: .FORCE_DEBUG
	$(TEST_CMD)

prod: .FORCE_PROD

.FORCE_DEBUG:
	zig build -p build -Doptimize=Debug

.FORCE_PROD:
	zig build -p build -Doptimize=ReleaseFast --release=fast
