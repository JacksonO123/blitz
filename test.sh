function run() {
  make "$1" 2> ./logs/"$1"
  OUT=$(echo $?)
  if [ $OUT == 0 ]; then
    echo "SUCCESS: $1"
  else
    echo "FAIL: $1"
  fi
}

while IFS="" read -r p || [ -n "$p" ]
do
  run "$p" &
done < input/working.txt
