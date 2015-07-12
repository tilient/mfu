#########################################################################
##
## sudo apt-get install libcurl3
## sudo ln -s /lib/x86_64-linux-gnu/libssl.so.1.0.0 /usr/lib/libssl.so
##
## nim c -d:release --threads:on mfu.nim
##
## nice -n 19 ionice -c 3 ./mfu
##
#########################################################################
##
## ToDo
##
## - check partial upload
## - debug -> loglevel 9
## - directory quick check
##   - foler-key cache
##
#########################################################################

import strutils, tables, math, times, os, streams,
       posix, libcurl, threadpool, locks

#########################################################################
## Parameters
#########################################################################

const maxNrOfUploaders = 24

type Parameters = tuple[appid         : string,
                        apikey        : string,
                        email         : string,
                        passwd        : string,
                        dirs          : seq[string],
                        debug         : bool,
                        quick         : bool,
                        nrOfUploaders : int,
                        loglevel      : int]

var params {.threadvar.} : Parameters

proc setParameter(cmd, val:string) {.thread.} =
  case cmd
    of "appid", "app-id"    : params.appid = val
    of "apikey", "api-key"  : params.apikey = val
    of "email", "e-mail"    : params.email = val
    of "passwd", "password" : params.passwd = val
    of "dir", "directory"   : params.dirs.add(val)
    of "dbg", "debug"       : params.debug = ("on" == val)
    of "quick"              : params.quick = ("on" == val)
    of "uploaders", "nr-of-uploaders" :
         params.nrOfUploaders = min(maxNrOfUploaders, parseInt(val))
    of "loglevel", "log-level" :
         params.loglevel = parseInt(val)

proc setParameterLine(line:string) {.thread.} =
  var spl = line.find(':')
  if spl < 0:
    spl = line.find('=')
  let
    cmd  = line[0 .. (spl-1)].strip()
    val = line[(spl+1) .. line.high].strip()
  setParameter(cmd, val)

proc loadParameters(filename:string) {.thread.} =
  if filename.existsFile():
    for line in filename.lines:
      setParameterLine(line)

proc loadCommandLineParameters() {.thread.} =
  for clp in commandLineParams():
    let cmdline = if   clp[0..1] == "--" : clp[2..clp.high]
                  elif clp[0] == '-'     : clp[1..clp.high]
                  else : clp
    setParameterLine(cmdline)

proc initParameters() {.thread.} =
  params.dirs = @[]
  params.nrOfUploaders = 4
  params.logLevel = 2
  params.debug = false
  params.quick = false
  loadParameters("/" / "etc" / "mfu" / "config")
  loadParameters(expandTilde("~" / ".mfu" / "config"))
  loadParameters("config")
  loadCommandLineParameters()

#########################################################################
## Channels
#########################################################################

var thrIx {.threadvar.} : int

var
  logCh    : TChannel[string]
  uploadCh : TChannel[string]
  resultCh : TChannel[string]
  shaRequestCh : TChannel[tuple[ix : int, path : string]]
  shaResultCh  : array[1 .. maxNrOfUploaders, TChannel[string]]

#########################################################################
## libSSL - SHA1 & SHA256
#########################################################################

proc SSL_library_init(): cint
     {.cdecl, dynlib: "libssl.so", importc.}

proc CRYPTO_num_locks(): int
     {.cdecl, dynlib: "libssl.so", importc.}

proc CRYPTO_set_locking_callback(funptr : pointer)
     {.cdecl, dynlib: "libssl.so", importc.}

discard SSL_library_init()
var LibSslLocks : seq[TLock]
LibSslLocks.newSeq(CRYPTO_num_locks())

proc openSslLockingCallback(mode : cint, lock_num : cint,
                            file : cstring, line : cint) {.cdecl .} =
  if 1 == (mode and 1):
    acquire(LibSslLocks[lock_num])
  else:
    release(LibSslLocks[lock_num])

CRYPTO_set_locking_callback(openSslLockingCallback)

#########################################################################

proc SHA1(d: cstring, n: culong, md: cstring = nil): cstring
         {.cdecl, dynlib: "libssl.so", importc.}

const SHA1Len = 20

proc sha1(s: string): string  =
  result = newStringOfCap(1 + 2 * SHA1Len)
  let md = newString(1 + SHA1Len)
  discard SHA1(s.cstring, s.len.culong, md.cstring)
  for i in 0 .. < SHA1Len:
    result.add(md[i].BiggestInt.toHex(2).toLower)

#########################################################################

proc SHA256(d: cstring, n: culong, md: cstring = nil): cstring
           {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Init (c: ptr): int
                 {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Update (c : ptr, data : cstring, n : culong): int
                   {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Final (md : cstring, c : ptr): int
                  {.cdecl, dynlib: "libssl.so", importc.}

const SHA256Len = 32

proc sha256(s: string): string  =
  result = newStringOfCap(1 + 2 * SHA256Len)
  let md = newString(1 + SHA256Len)
  discard SHA256(s.cstring, s.len.culong, md.cstring)
  for i in 0 .. < SHA256Len:
    result.add(md[i].BiggestInt.toHex(2).toLower)

proc SHA256_Sum(filename : string): string =
  result = newStringOfCap(1 + 2 * SHA256Len)
  var f : File
  if f.open(filename):
    const bufSize = 2048
    let
      ctx = cast[ptr char](alloc0(128))
      md  = newString(1 + SHA256Len)
      buf = newString(bufSize)
    discard SHA256_Init(ctx)
    while not f.endOfFile():
      let bytesRead = f.readBuffer(buf.cstring, bufSize)
      discard SHA256_Update(ctx, buf.cstring, bytesRead.culong)
    f.close()
    discard SHA256_Final(md.cstring, ctx)
    dealloc(ctx)
    for i in 0 .. < SHA256Len:
      result.add md[i].BiggestInt.toHex(2).toLower

#########################################################################
## CURL
#########################################################################

var curl_collect_str {.threadvar.} : string

proc collect_str(buffer: cstring; size: int; nitems: int;
                 outstream: pointer): int {.cdecl.} =
  let
    str : string = $buffer
    length = nitems * size
  curl_collect_str.add(str.substr(0, length - 1))
  length

proc geturl(url : string, timeout : int = 120000,
            headerlist : Pslist = nil,
            formpost : Phttppost = nil): string =
  let curl = easy_init()
  if headerlist != nil:
    discard curl.easy_setopt(OPT_HTTPHEADER, headerlist)
  if formpost != nil:
    discard curl.easy_setopt(OPT_HTTPPOST, formpost)
  discard curl.easy_setopt(OPT_NOSIGNAL, true)
  discard curl.easy_setopt(OPT_FOLLOWLOCATION, 1)
  discard curl.easy_setopt(OPT_WRITEFUNCTION, collect_str)
  discard curl.easy_setopt(OPT_FAILONERROR, 1)
  if params.debug:
    discard curl.easy_setopt(OPT_VERBOSE, 1)
  discard curl.easy_setopt(OPT_DNS_CACHE_TIMEOUT, 600)
  discard curl.easy_setopt(OPT_URL, url)
  discard curl.easy_setopt(OPT_TIMEOUT, timeout div 1000)
  curl_collect_str = ""
  discard curl.easy_perform()
  curl.easy_cleanup()
  if params.debug:
    echo "--- DEBUG ---------------------------------------------------"
    echo curl_collect_str
    echo "=== DEBUG ==================================================="
  result = curl_collect_str

proc posturl(url : string, filename : string,
             buffer : string, headers : string = "",
             timeout : int = 120000): string =
  var
    form_headers : Pslist = nil
    formpost     : Phttppost = nil
    lastpost     : Phttppost = nil
    headerlist   : Pslist = nil
  form_headers = form_headers.slist_append("Content-type: ???/???")
  discard formadd(addr formpost, addr lastpost,
                  FORM_CONTENTHEADER, form_headers,
                  FORM_COPYNAME, filename.cstring,
                  FORM_BUFFER, filename.cstring,
                  FORM_BUFFERPTR, buffer.cstring,
                  FORM_BUFFERLENGTH, buffer.len,
                  FORM_END)
  headerlist = headerlist.slist_append("Expect:")
  headerlist = headerlist.slist_append(headers)
  result = geturl(url, timeout, headerlist, formpost)
  formfree(formpost)

#########################################################################
## Tools
#########################################################################

proc string_between(str, start_str, end_str : string): string =
  let
    pos       = str.find(start_str)
    start_pos = pos + start_str.len()
    end_pos   = if (pos >= 0): str.find(end_str, start_pos) - 1 else: -1
  if end_pos >= 0:
    str.substr(start_pos, end_pos)
  else:
    "???"

proc xml_value(str, key : string): string =
  str.string_between("<" & key & ">", "</" & key & ">")

proc xml_int_value(str, key : string): int =
  parseInt(xml_value(str, key))

proc xml_value_equals(str, key, value : string): bool =
  value == xml_value(str, key)

template try3times(msg : string, actions : stmt): stmt {.immediate.} =
  var retry_count = 3
  while retry_count > 0:
    try:
      actions
      retry_count = 0
    except:
      retry_count -= 1
      if retry_count > 0:
        log(3, "ERR retry", msg)
        sleep(15000)
      else:
        log(1, "***ERR***", msg)

type KVs = openArray[tuple[key : string, value : string]]

proc urlEncode(str: string): string =
  result = newStringOfCap(2 * str.len)
  for ch in str:
    case ch
      of 'a'..'z', 'A'..'Z', '0'..'9', '_':
        result.add(ch)
      of ' ':
        result.add('+')
      else:
        result.add('%')
        result.add(ch.ord().toHex(2))

proc fileLines (filename: string): seq[string] =
  result = @[]
  for line in filename.lines:
    result.add(line)

#########################################################################
## Logging
#########################################################################

proc logLoop(x:int) {.thread.} =
  initParameters()
  var nextTime = -999.999
  var cnt = 0
  while true:
    stdout.write logCh.recv()
    let now = epochTime()
    if now > nextTime:
      stdout.write("|/-\\"[cnt])
      stdout.write(char(13))
      cnt = (cnt + 1) mod 4
      nextTime = now + 0.2
    stdout.flushFile()

proc log(level : int, mark : string, str : string) =
  logCh.send(if level <= params.logLevel:
               "[" & getClockStr() & " " &  mark.align(9) & "]" &
               " " & str & "\n"
             else:
               "")

#########################################################################
## SHA256 Cache
#########################################################################

proc SHA256ofFileLoop(x:int) =
  initParameters()
  var
    fileSHA256Cache = initTable[string, string](256)
    filenameFileSHA256Cache = ".mfu_sha_cache"
  if not existsFile(filenameFileSHA256Cache):
    filenameFileSHA256Cache = expandTilde("~" / ".mfu_sha_cache")
  if not existsFile(filenameFileSHA256Cache):
    filenameFileSHA256Cache.open(fmWrite).close()
  for line in filenameFileSHA256Cache.lines:
    let
      key = line[0..39]
      val = line[41..104]
    fileSHA256Cache[key] = val
  while true:
    let (ix,filePath) = shaRequestCh.recv()
    let
      mod_time = filePath.getLastModificationTime().toSeconds().toInt()
      mod_time_str = $mod_time
      file_size = filePath.getFileSize()
      file_size_str = $file_size
      sha = sha1(mod_time_str & "_" & file_size_str & "_" & filePath)
    var res = ""
    if fileSHA256Cache.hasKey(sha):
      if params.quick:
        res = "quick"
      else:
        res = fileSHA256Cache[sha]
    else:
      res = SHA256_Sum(filePath)
      fileSHA256Cache[sha] = res
      var f = open(filenameFileSHA256Cache, fmAppend)
      f.write(sha)
      f.write(":")
      f.write(res)
      f.write("\n")
      f.close()
    shaResultCh[ix].send(res)

proc SHA256ofFile(filePath : string): string =
  shaRequestCh.send((thrIx,filePath))
  shaResultCh[thrIx].recv()

#########################################################################
## Mediafire Session ID
#########################################################################

proc mediafire_call_raw(op : string, params : KVs): string =
  var url = "https://www.mediafire.com/api/1.3/" & op &
            ".php?token_version=1"
  for param in items(params):
    url &= "&" & param.key & "=" & param.value
  geturl(url)

var
  sessionId {.threadvar.} : string
  mfs_time  {.threadvar.} : float
  signature {.threadvar.} : string

proc initSessionId() {.thread.} =
  sessionId = "NN"
  mfs_time = -999.99
  signature = sha1(params.email & params.passwd &
                   params.appid & params.apikey)

proc getSessionID() : string {.thread.} =
  let
    now = epochTime()
    age = now - mfs_time
  if age > 400.0 :
    sessionId = mediafire_call_raw("user/get_session_token",
          {"signature"       : signature,
           "email"           : params.email,
           "password"        : params.passwd,
           "application_key" : params.apikey,
           "application_id"  : params.appid}).xml_value("session_token")
    mfs_time = now
    log(4, "new ses", sessionId)
  sessionID

#########################################################################
## MediaFire API
#########################################################################

proc mediafire_call(op : string, params : KVs): string =
  var url = "https://www.mediafire.com/api/1.3/" & op &
            ".php?token_version=1&session_token=" & getSessionID()
  for param in items(params):
    url &= "&" & param.key & "=" & param.value
  geturl(url)

proc mediafire_xml_value(key, op : string, params : KVs): string =
  mediafire_call(op, params).xml_value(key)

#########################################################################
## MediaFire Bitmap Decoding
#########################################################################

proc decode_bitmap(str : string, nr_of_bits : int): string =
  let s = str.xml_value("words").replace("</word><word>", ",")
           .replace("<word>").replace("</word>").split(',').map parseInt
  var binstr = newStringOfCap(1 + 16 * s.len)
  for ix in countdown(high(s), 0):
    binstr.add(s[ix].toBin(16))
  result = newStringOfCap(2 + high(binstr))
  for ix in countdown(high(binstr), 0):
    result.add binstr[ix]
  result = result[0 .. (nr_of_bits-1)]

#########################################################################
## MediaFire Files
#########################################################################

proc file_needs_update(file_path, file_size : string,
                       nr_of_units, unit_size : var int,
                       hash_exists : var bool,
                       bitmapstr : var string,
                       file_sig : var string): bool =
  file_sig = SHA256ofFile(file_path)
  if file_sig == "quick":
    result = false
  else:
    let
      (dir_path, filename) = splitPath(file_path)
      res = mediafire_call("upload/check",
                           {"filename"  : urlEncode(filename),
                            "resumable" : "yes",
                            "size"      : file_size,
                            "hash"      : file_sig,
                            "path"      : urlEncode(dir_path)})
      file_exists = res.xml_value_equals("file_exists", "yes")
      same_hash   = res.xml_value_equals("different_hash", "no")
    hash_exists = res.xml_value_equals("hash_exists", "yes")
    nr_of_units = res.xml_int_value("number_of_units")
    unit_size = res.xml_int_value("unit_size")
    bitmapstr = decode_bitmap(res, nr_of_units)
    result = not (file_exists and same_hash)

proc instant_upload(dir_path, filename,
                    file_size, file_sig : string) =
  discard mediafire_call("upload/instant",
                         {"size"     : file_size,
                          "hash"     : file_sig,
                          "action_on_duplicate" : "replace",
                          "filename" : urlEncode(filename),
                          "path"     : urlEncode(dir_path)})

proc simple_upload(path, dir_path, filename: string, file_size: int64) =
  let
    headers = "x-filesize:"  & $file_size
    url     = "https://www.mediafire.com/api/1.3/" &
              "upload/simple.php?token_version=1" &
              "&session_token=" & getSessionID() &
              "&action_on_duplicate=replace" &
              "&path=" & urlEncode(dir_path)
    fstr    = newFileStream(path, fmRead)
    bytes   = fstr.readStr(int(file_size))
  fstr.close()
  discard posturl(url, filename, bytes, headers)

proc resumable_upload(path, dir_path, filename, file_size,
                      file_sig : string, nr_of_units, unit_size : int,
                      bitmapstr : var string) =
  let fstr = newFileStream(path, fmRead)
  defer: fstr.close()
  ## big hack !!
  if bitmapstr.find('0') < 0: bitmapstr[0] = '0'
  ## ## ## ## ## ##
  var upload_key = ""
  for ix in 0 .. < nr_of_units:
    let
      bytes    = fstr.readStr(unit_size)
      progress = intToStr(1+ix, 2) & "/" & intToStr(nr_of_units, 2)
    if bitmapstr[ix] == '1' :
      log(3, "--- " & progress, path)
    else:
      log(3, "upl " & progress, path)
      let
        headers = "x-unit-hash:" & sha256(bytes) & "\c\L" &
                  "x-filehash:"  & file_sig      & "\c\L" &
                  "x-filesize:"  & $file_size    & "\c\L" &
                  "x-unit-id:"   & $ix           & "\c\L" &
                  "x-unit-size:" & $bytes.len
        url = "https://www.mediafire.com/api/1.3/" &
              "upload/resumable.php?token_version=1" &
              "&session_token=" & getSessionID() &
              "&resumable=yes&action_on_duplicate=replace" &
              "&path=" & urlEncode(dir_path)
        res = posturl(url, filename, bytes, headers)
      upload_key = xml_value(res, "key")
      bitmapstr = decode_bitmap(res, nr_of_units)
  if params.debug:
    discard mediafire_call("upload/poll_upload", {"key" : upload_key})
  doAssert(bitmapstr.find('0') < 0)

proc upload_file_raw(path : string) =
  let
    file_size     = getFileSize(path)
    file_size_str = $file_size
  var
    nr_of_units = 0
    unit_size   = 0
    hash_exists = false
    bitmapstr   = ""
    file_sig    = ""
  log(3, "check", path)
  if file_size <= 0:
    log(1, "*error*", path)
    log(1, "*** MF Bug ***", "zero length files are not supported")
  elif file_needs_update(path, file_size_str, nr_of_units,
                         unit_size, hash_exists, bitmapstr, file_sig):
    let
      (dir_path, name) = splitPath(path)
    if hash_exists:
      log(2, "quick upl", path)
      instant_upload(dir_path, name, file_size_str, file_sig)
    elif (file_size < 4200000):
      log(2, "simpl upl", path)
      simple_upload(path, dir_path, name, file_size)
    else:
      log(2, "upload", path)
      resumable_upload(path, dir_path, name, file_size_str,
                       file_sig, nr_of_units, unit_size, bitmapstr)

#########################################################################
## File Uploaders
#########################################################################

proc fileUploader(ix : int) {.thread.} =
  thrIx = ix
  initParameters()
  os.sleep(random(200))
  initSessionId()
  while true:
    resultCh.send("done")
    let filePath = uploadCh.recv()
    try3times(filePath):
      upload_file_raw(filePath)

proc upload_file(path : string) =
  discard resultCh.recv()
  uploadCh.send(path)

#########################################################################
## MediaFire Directories
#########################################################################

proc create_mediafire_folder(path, key : string): string {.gcsafe.}

proc create_mediafire_root_folder(path : string): string =
  let (dir_path, name) = splitPath(path)
  if dir_path == "":
    mediafire_xml_value(
      "folder_key", "folder/create",
      {"foldername" : urlEncode(name),
       "action_on_duplicate" : "replace"})
  else:
    create_mediafire_folder(
      path, create_mediafire_root_folder(dir_path))

proc create_mediafire_folder(path, key : string): string =
  if key == "":
    create_mediafire_root_folder(path)
  else:
    mediafire_xml_value(
      "folder_key", "folder/create",
      {"foldername"          : urlEncode(splitPath(path).tail),
       "action_on_duplicate" : "replace",
       "parent_key"          : key})

proc upload_directory(path : string, key : string = "") =
  try3times(path):
    log(3, "dir", path)
    let dir_key = create_mediafire_folder(path, key)
    for kind, path in walkDir(path):
      case kind:
        of pcDir  : upload_directory(path, dir_key)
        of pcFile : upload_file(path)
        else      : discard

#########################################################################
## Main
#########################################################################

proc main =
  var
    log_thr  : TThread[int]
    sha_thr  : TThread[int]
    upl_thrs : array[1 .. maxNrOfUploaders, TThread[int]]
  signal(SIGPIPE, SIG_IGN)
  thrIx = 0
  createDir(expandTilde("~" / ".mfu"))
  initParameters()
  logCh.open()
  createThread(log_thr, logLoop, 0)
  log(2, "start", getDateStr())
  initSessionId()
  shaRequestCh.open()
  for ix in 1 .. params.nrOfUploaders:
    shaResultCh[ix].open()
  createThread(sha_thr, SHA256ofFileLoop, 0)
  uploadCh.open()
  resultCh.open()
  for ix in 1..params.nrOfUploaders:
    createThread(upl_thrs[ix], fileUploader, ix)

  for dir in params.dirs:
    upload_directory(dir)
  for ix in 1 .. params.nrOfUploaders:
    discard resultCh.recv()
  uploadCh.close()
  resultCh.close()
  shaRequestCh.close()
  for ix in 1 .. params.nrOfUploaders:
    shaResultCh[ix].close()
  logCh.close()
  if 2 <= params.logLevel:
    stdout.write("[" & getClockStr() & "      done] " &
                 getDateStr() & "\n")

when isMainModule:
  main()

#########################################################################
