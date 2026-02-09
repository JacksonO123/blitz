#! /usr/bin/env bash

export PATH="$BLITZ_DIR/zig-out/bin:$PATH"

alias test-blitz-diffs="zig run src/test_diffs.zig --"
