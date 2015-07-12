import strutils

type
  StringChannel = TChannel[string]

var
  channels : array[0..3, StringChannel]
  thr: array [0..3, TThread[int]]

proc consumer(ix : int) {.thread.} =
  echo channels[ix].recv()
  channels[ix].send("fighters")

proc main =
  for ix in 0..3: channels[ix].open()
  for ix in 0..3: createThread(thr[ix], consumer, ix)
  for ix in 0..3: channels[ix].send("foo (" & intToStr(ix) & ")")
  joinThreads(thr)
  for ix in 0..3: echo channels[ix].recv()
  for ix in 0..3: channels[ix].close()

when isMainModule:
  main()
