function run() {
  if [ "$2" == "-l" ]; then
    make "$1" 2> ./logs/"$1"
  else
    make "$1"
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
    run "$p" -l &
  done < input/working.txt
elif [ "$1" == "build" ]; then
  echo "build is a builtin test command, if you wanted to run a\nblitz file named 'build', consider renaming the file"
  zig build -p build
else
  run "$1.blitz"
fi

