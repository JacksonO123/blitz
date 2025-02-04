function run() {
  zig run src/compiler.zig -- "input/$1"
}

function run_log() {
  if [ "$2" == "-l" ]; then
    run "$1" 2> ./logs/"$1"
  else
    run "$1"
  fi

  OUT=$(echo $?)

  if [ $OUT == 0 ]; then
    echo "SUCCESS: $1"
  else
    echo "FAIL: $1"
  fi
}

function run_dbg_build() {
  blitzc-debug "input/$1"
}

function run_log_dbg_build() {
  if [ "$2" == "-l" ]; then
    run_dbg_build "$1" 2> ./logs/"$1"
  else
    run_dbg_build "$1"
  fi

  OUT=$(echo $?)

  if [ $OUT == 0 ]; then
    echo "SUCCESS: $1"
  else
    echo "FAIL: $1"
  fi
}

if [ "$1" == "" ]; then
  while IFS="" read -r p || [ -n "$p" ]
  do
    run_log_dbg_build "$p" -l &
  done < input/working.txt
else
  if [[ "$1" == *\.blitz ]]; then
    echo "expected filename without \".blitz\" extension"
    exit 0
  fi

  run_log "$1.blitz"
fi

wait
