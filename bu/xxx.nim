
import threadpool

type StringChannel = TChannel[string]
var channels : array[1..3, StringChannel]

proc consumer(ix : int) {.thread.} =
  echo channels[ix].recv() ###### not GC-safe: 'channels'

proc main =
  for ix in 1..3: channels[ix].open()
  for ix in 1..3: spawn consumer(ix)
  for ix in 1..3: channels[ix].send("test")
  sync()
  for ix in 1..3: channels[ix].close()

when isMainModule:
  main()

