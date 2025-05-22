function test_tokenize() {
  zig run src/test-tokenize.zig -- "input/$1.blitz"
}

# use multiple methods to check if in project root
if [[ "$(pwd)" == *\/blitz ]] && [ -e "$(pwd)/build.zig" ] && [ -e "$(pwd)/.zig-cache" ]; then
  if [[ "$1" == *\.blitz ]]; then
    echo "expected filename without [.blitz] extension"
    exit 0
  fi

  test_tokenize "$1"
else
  echo "expected to be in project root"
  exit 1
fi

wait
