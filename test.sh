function run() {
  blitz-debug "input/$1"
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

if [ "$1" == "" ]; then
  while IFS="" read -r p || [ -n "$p" ]
  do
    run_log "$p" -l &
  done < input/working.txt
else
  if [[ "$1" == *\.blitz ]]; then
    echo "expected filename without \".blitz\" extension"
    exit 0
  fi

  run_log "$1.blitz"
fi

