#########################################################################
##
## nim c -d:release --threads:on mfu.nim
## ./mfu
##
#########################################################################

import strutils, times, os, streams, posix, libcurl

#########################################################################
## Parameters
#########################################################################

const appid   = "44501"
const apikey  = "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff"
const email   = "wiffel@tilient.net"
const passwd  = "ttT1l1ent"

#########################################################################
## libssl - SHA1
#########################################################################

proc SHA1(d: cstring, n: culong, md: cstring = nil): cstring
         {.cdecl, dynlib: "libssl.so", importc.}

#########################################################################

const SHA1Len = 20

proc SHA1(s: string): string  =
  result = ""
  var s = SHA1(s.cstring, s.len.culong)
  for i in 0 .. < SHA1Len:
    result.add s[i].BiggestInt.toHex(2).toLower

#########################################################################
## libssl - SHA256
#########################################################################

proc SHA256(d: cstring, n: culong, md: cstring = nil): cstring
           {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Init (c: ptr): int
                 {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Update (c : ptr, data : cstring, n : culong): int
                   {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Final (md : cstring, c : ptr): int
                  {.cdecl, dynlib: "libssl.so", importc.}

#########################################################################

const SHA256Len = 32

proc SHA256(s: string): string =
  var res = SHA256(s.cstring, s.len.culong)
  result = ""
  for i in 0 .. < SHA256Len:
    result.add res[i].BiggestInt.toHex(2).toLower

proc SHA256_Sum(filename : string): string =
  const sha256_bufsize = 2048
  result = newStringOfCap(1 + 2 * SHA256Len)
  var f : File
  if f.open(filename):
    defer: f.close()
    let
      buf = newString(sha256_bufsize)
      md  = newString(1 + SHA256Len)
      ctx = cast[ptr char](alloc0(128))
    defer: dealloc(ctx)
    discard SHA256_Init(ctx)
    while not endOfFile(f):
      let bytesRead = readBuffer(f, buf.cstring, sha256_bufsize)
      discard SHA256_Update(ctx, buf.cstring, bytesRead.culong)
    discard SHA256_Final(md.cstring, ctx)
    for i in 0 .. < SHA256Len:
      result.add md[i].BiggestInt.toHex(2).toLower

#########################################################################
## Logging
#########################################################################

const logLevel = 4

proc log(level : int, mark : string, strs : varargs[string]) =
  if level <= logLevel:
    var str = "[" & mark.align(9) & "]"
    for item in items(strs):
      str &= " " & item
    stdout.write str & "\n"

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
        log(1, "ERR retry", msg)
        sleep(15000)
      else:
        log(1, "***ERR***", msg)

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
  defer: curl.easy_cleanup()
  if headerlist != nil:
    discard curl.easy_setopt(OPT_HTTPHEADER, headerlist)
  if formpost != nil:
    discard curl.easy_setopt(OPT_HTTPPOST, formpost)
  discard curl.easy_setopt(OPT_NOSIGNAL, true)
  discard curl.easy_setopt(OPT_FOLLOWLOCATION, 1)
  discard curl.easy_setopt(OPT_WRITEFUNCTION, collect_str)
  discard curl.easy_setopt(OPT_FAILONERROR, 1)
  discard curl.easy_setopt(OPT_URL, url)
  discard curl.easy_setopt(OPT_TIMEOUT, timeout div 1000)
  curl_collect_str = ""
  discard curl.easy_perform()
  result = curl_collect_str

proc posturl(url : string, filename : string,
             buffer : string, headers : string = "",
             timeout : int = 120000): string =
  var
    form_headers : Pslist = nil
    formpost     : Phttppost = nil
    lastpost     : Phttppost = nil
    headerlist   : Pslist = nil
  form_headers = form_headers.slist_append("Content-Type: ???/???")
  discard formadd(addr formpost, addr lastpost,
                  FORM_CONTENTHEADER, form_headers,
                  FORM_COPYNAME, filename.cstring,
                  FORM_BUFFER, filename.cstring,
                  FORM_BUFFERPTR, buffer.cstring,
                  FORM_BUFFERLENGTH, buffer.len,
                  FORM_END)
  defer: formfree(formpost)
  headerlist = headerlist.slist_append("Expect:")
  headerlist = headerlist.slist_append(headers)
  result = geturl(url, timeout, headerlist, formpost)

#########################################################################
## MediaFire Session
#########################################################################

type
  KVs = openArray[tuple[key : string, value : string]]

proc mediafire_call_raw(op : string, params : KVs): string
     {.gcsafe.}

type
  StringChannel = TChannel[string]

var
  mfs_channel_a : StringChannel
  mfs_channel_b : StringChannel
  mfs_thread : TThread[int]

proc sessionId_loop(x : int) {.thread.} =

  var
    mfs_str : string = "NN"
    mfs_time : float = -999.99

  proc update_mfs_id_if_needed() =
    let
      now = epochTime()
      age = now - mfs_time
    if age > 400.0 :
      let signature = SHA1(email & passwd & appid & apikey)
      mfs_time = now
      mfs_str = mediafire_call_raw("user/get_session_token",
                 {"signature"       : signature,
                  "email"           : email,
                  "password"        : passwd,
                  "application_key" : apikey,
                  "application_id"  : appid}).xml_value("session_token")
    mfs_channel_b.send(mfs_str)

  while true:
    let cmd : string = mfs_channel_a.recv()
    case cmd:
      of "stop"  : break
      of "getId" : update_mfs_id_if_needed()
      else       : discard
  mfs_channel_b.send("done")

proc sessionId_init =
  mfs_channel_a.open()
  mfs_channel_b.open()
  createThread(mfs_thread, sessionId_loop, 0)

proc sessionId_term =
  mfs_channel_a.send("stop")
  echo mfs_channel_b.recv()
# mfs_channel_b.close()
# mfs_channel_a.close()

proc mfsid(): string =
  mfs_channel_a.send("getId")
  result = mfs_channel_b.recv()

#########################################################################
## MediaFire API
#########################################################################

proc mediafire_call_raw(op : string, params : KVs): string =
  var url = "https://www.mediafire.com/api/1.3/" & op &
            ".php?token_version=1"
  for param in items(params):
    url &= "&" & param.key & "=" & param.value
  geturl(url)

proc mediafire_call(op : string, params : KVs): string =
  var url = "https://www.mediafire.com/api/1.3/" & op &
            ".php?token_version=1&session_token=" & mfsid()
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
  for num in s :
    binstr.add(num.toBin(16))
  result = newStringOfCap(2 + high(binstr))
  for i in countdown(high(binstr), 0):
    result.add binstr[i]
  result = result[0 .. (nr_of_bits-1)]

#########################################################################
## MediaFire Files
#########################################################################

proc file_needs_update(file_path, file_size : string,
                       nr_of_units, unit_size : var int,
                       hash_exists : var bool,
                       bitmapstr : var string,
                       file_sig : var string): bool =
  file_sig = SHA256_Sum(file_path)
  let
    (dir_path, filename) = splitPath(file_path)
    res = mediafire_call("upload/check",
                         {"filename"  : filename,
                          "resumable" : "yes",
                          "size"      : file_size,
                          "hash"      : file_sig,
                          "path"      : dir_path})
    file_exists = res.xml_value_equals("file_exists", "yes")
    same_hash   = res.xml_value_equals("different_hash", "no")
  hash_exists = res.xml_value_equals("hash_exists", "yes")
  nr_of_units = res.xml_int_value("number_of_units")
  unit_size = res.xml_int_value("unit_size")
  bitmapstr = decode_bitmap(res, nr_of_units)
  not (file_exists and same_hash)

proc instant_upload(dir_path, filename,
                    file_size, file_sig : string) =
  discard mediafire_call("upload/instant",
                         {"size"     : file_size,
                          "hash"     : file_sig,
                          "filename" : filename,
                          "path"     : dir_path})

proc resumable_upload(path, dir_path, filename, file_size,
                      file_sig : string, nr_of_units, unit_size : int,
                      bitmapstr : var string) =
  let fstr = newFileStream(path, fmRead)
  defer: fstr.close()
  ## big hack !!
  if bitmapstr.find('0') < 0: bitmapstr[0] = '0'
  ## ## ## ## ## ##
  for ix in 0 .. < nr_of_units:
    let
      bytes    = fstr.readStr(unit_size)
      progress = intToStr(1+ix, 2) & "/" & intToStr(nr_of_units, 2)
    if bitmapstr[ix] == '1' :
      log(2, "--- " & progress, path)
    else:
      log(2, "upl " & progress, path)
      let
        headers = "x-unit-hash:" & SHA256(bytes) & "\c\L" &
                  "x-filehash:"  & file_sig      & "\c\L" &
                  "x-filesize:"  & $file_size    & "\c\L" &
                  "x-unit-id:"   & $ix           & "\c\L" &
                  "x-unit-size:" & $bytes.len
        url = "https://www.mediafire.com/api/1.3/" &
              "upload/resumable.php?token_version=1" &
              "&session_token=" & mfsid() &
              "&resumable=yes&action_on_duplicate=replace" &
              "&path=" & dir_path
      bitmapstr = decode_bitmap(posturl(url, filename, bytes, headers),
                                nr_of_units)

proc upload_file_raw(path : string) =
  let
    file_size        = getFileSize(path)
    file_size_str    = $file_size
  var
    nr_of_units = 0
    unit_size   = 0
    hash_exists = false
    bitmapstr   = ""
    file_sig    = ""
  log(2, "check", path)
  try3times(path):
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
      else:
        log(2, "upload", path)
        resumable_upload(path, dir_path, name, file_size_str,
                         file_sig, nr_of_units, unit_size, bitmapstr)

#########################################################################
##  Uploaders
#########################################################################

const
  nrOfUploaders = 4
var
  upload_channel : StringChannel
  upload_channel_b : StringChannel
  upload_threads : array[1..nrOfUploaders,TThread[int]]

proc upload_loop(i:int) {.thread.} =
  while true:
    let cmd : string = upload_channel.recv()
    case cmd:
      of "stop"  : break
      else       : upload_file_raw(cmd)
  echo "---done---"
  upload_channel_b.send("Done")
  sleep(60000)
  echo "-*-done---"

proc uploaders_init =
  upload_channel.open()
  upload_channel_b.open()
  for ix in 1 .. nrOfUploaders:
    createThread(upload_threads[ix], upload_loop, 0)

proc uploaders_term =
  for ix in 1 .. nrOfUploaders:
    upload_channel.send("stop")
  echo "++++++++"
  for ix in 1 .. nrOfUploaders:
    echo upload_channel_b.recv()
# joinThreads(upload_threads)
  echo "+--+++++"
# upload_channel.close()
# upload_channel_b.close()

proc upload_file(path : string) =
  upload_channel.send(path)

#########################################################################
## MediaFire Directories
#########################################################################

proc create_mediafire_folder(path, key : string): string {.gcsafe.}

proc create_mediafire_root_folder(path : string): string =
  let (dir_path, name) = splitPath(path)
  if dir_path == "":
    log(2, "dir", path)
    mediafire_xml_value(
      "folder_key", "folder/create",
      {"foldername" : name, "action_on_duplicate" : "replace"})
  else:
    create_mediafire_folder(
      path, create_mediafire_root_folder(dir_path))

proc create_mediafire_folder(path, key : string): string =
  if key == "":
    create_mediafire_root_folder( path)
  else:
    log(2, "dir", path)
    mediafire_xml_value("folder_key",
                        "folder/create",
                        {"foldername"          : splitPath(path).tail,
                         "action_on_duplicate" : "replace",
                         "parent_key"          : key})

proc upload_directory(path : string, key : string = "") =
  try3times(path):
    let dir_key = create_mediafire_folder(path, key)
    for kind, path in walkDir(path):
      case kind:
        of pcDir  : upload_directory(path, dir_key)
        of pcFile : upload_file(path)
        else      : discard

#########################################################################
## Main
#########################################################################

proc main() =
  signal(SIGPIPE, SIG_IGN)
  sessionId_init()
  uploaders_init()
# const dirs = ["/backups"]
# const dirs = ["/home/wiffel/dev/mediafire"]
  const dirs = ["/home/wiffel/kashbah/xxx"]
  for dir in dirs:
    upload_directory(dir)
  uploaders_term()
  sessionId_term()

when isMainModule:
  main()

#########################################################################
