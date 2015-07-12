import os

let s = "|/-\\|/-\\"
var cnt = 0

proc nnn() =
  var str = "  "
  str[0] = s[cnt]
  str[1] = char(13)
  stdout.write str
  flushFile(stdout)
  cnt = (cnt + 1) mod 8
  sleep(100)

for ix in 1 .. 20:
  nnn()

stdout.write "wiffel"


echo expandTilde("/" / "wiffel")
echo "/" / "wiffel"
