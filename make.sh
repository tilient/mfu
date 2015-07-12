nim c -d:release --threads:on --verbosity:0 -w:off --hints:off --deadCodeElim:on mfu.nim
#nim c --debuginfo --lineDir:on --threads:on  mfu.nim
strip mfu
upx -q --best mfu

