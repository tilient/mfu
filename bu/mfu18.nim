#########################################################################
##
## nim c -d:release -d:ssl --verbosity:0 -w:off--threads:on mfu.nim
## ./mfu
##
#########################################################################

import strutils, times, httpclient, os, streams, posix, threadpool

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

proc SHA1(s: string): string =
  result = ""
  var s = SHA1(s.cstring, s.len.culong)
  for i in 0 .. < SHA1Len:
    result.add s[i].BiggestInt.toHex(2).toLower
  dealloc(s)

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
  result = ""
  let s = SHA256(s.cstring, s.len.culong)
  for i in 0 .. < SHA256Len:
    result.add s[i].BiggestInt.toHex(2).toLower
  dealloc(s)

proc SHA256_Sum(filename : string): string =
  const sha256_bufsize = 2048
  result = newStringOfCap(1 + 2 * SHA256Len)
  var f : File
  if f.open(filename):
    defer: f.close()
    let
      ctx = cast[ptr char](alloc0(128))
      buf = newString(sha256_bufsize)
      md  = newString(1 + SHA256Len)
    discard SHA256_Init(ctx)
    while not endOfFile(f):
      let bytesRead = readBuffer(f, buf.cstring, sha256_bufsize)
      discard SHA256_Update(ctx, buf.cstring, bytesRead.culong)
    discard SHA256_Final(md.cstring, ctx)
    dealloc(ctx)
    for i in 0 .. < SHA256Len:
      result.add md[i].BiggestInt.toHex(2).toLower

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
        echo "[ERR retry] " & msg
        sleep(15000)
      else:
        echo "[***ERR***] " & msg

#########################################################################
## MediaFire Session
#########################################################################

proc mediafire_xml_value(key : string, op : string,
                    params : varargs[array[2,string]]): string {.gcsafe.}

type MFS = tuple [str : string, time : float]
var mfs {.threadvar.} : MFS

proc mfsid(): string =
  let
    now = epochTime()
    age = now - mfs.time
  if age > 400.0 :
    let signature = SHA1(email & passwd & appid & apikey)
    mfs.time = now
    mfs.str = mediafire_xml_value("session_token",
                                  "user/get_session_token",
                                  ["signature", signature],
                                  ["email", email],
                                  ["password", passwd],
                                  ["application_key", apikey],
                                  ["application_id", appid])
  mfs.str

#########################################################################
## MediaFire API
#########################################################################

proc mediafire_call(op : string,
                    params : varargs[array[2,string]]): string =
  var url = "https://www.mediafire.com/api/1.3/" & op &
            ".php?token_version=1&session_token=" & mfsid()
  for param in items(params):
    url &= "&" & param[0] & "=" & param[1]
  result = getContent(url, timeout = 120000)

proc mediafire_xml_value(key : string, op : string,
                         params : varargs[array[2,string]]): string =
  mediafire_call(op, params).xml_value(key)

#########################################################################
## MediaFire Bitmap Decoding
#########################################################################

proc decode_bitmap(str : string): string =
  let
    s1 = str.xml_value("words").replace("</word><word>", ",")
    s2 = s1.replace("<word>").replace("</word>")
    s  = s2.split(',').map parseInt
  var binstr = ""
  for num in s :
    binstr &= num.toBin(16)
  result = ""
  for i in countdown(high(binstr), 0):
    result.add binstr[i]

#########################################################################
## MediaFire Files
#########################################################################

proc file_needs_update(file_path, file_size, file_sig : string,
                       nr_of_units, unit_size : var int,
                       hash_exists : var bool,
                       bitmapstr : var string): bool =
  let
    (dir_path, filename) = splitPath(file_path)
    res = mediafire_call("upload/check",
                             ["filename", filename],
                             ["resumable", "yes"],
                             ["size", file_size],
                             ["hash", file_sig],
                             ["path", dir_path])
    file_exists = res.xml_value_equals("file_exists", "yes")
    same_hash   = res.xml_value_equals("different_hash", "no")
  hash_exists = res.xml_value_equals("hash_exists", "yes")
  nr_of_units = res.xml_int_value("number_of_units")
  unit_size = res.xml_int_value("unit_size")
  bitmapstr = decode_bitmap(res)
  not (file_exists and same_hash)

proc instant_upload(dir_path, filename,
                    file_size, file_sig : string) =
  discard mediafire_call("upload/instant",
                             ["size", file_size],
                             ["hash", file_sig],
                             ["filename", filename],
                             ["path", dir_path])

proc resumable_upload(path, dir_path, filename, file_size,
                      file_sig : string, nr_of_units, unit_size : int,
                      bitmapstr : var string) =
  let fstr = newFileStream(path, fmRead)
  defer: fstr.close()
  for ix in 0 .. < nr_of_units:
    let
      bytes    = fstr.readStr(unit_size)
      progress = intToStr(1+ix, 2) & "/" & intToStr(nr_of_units, 2)
    if bitmapstr[ix] == '1' :
      echo "[--- " & progress & "] " & path
    else:
      echo "[upl " & progress & "] " & path
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
      var data = newMultipartData()
      data["file"] = (filename, "???/???", bytes)
      bitmapstr = decode_bitmap(
                    postContent(url, headers, multipart = data,
                                timeout = 120000))

proc upload_file(mediafire_session : MFS, path : string)  =
  let
    (dir_path, name) = splitPath(path)
    file_size        = getFileSize(path)
    file_size_str    = $file_size
    file_sig         = SHA256_Sum(path)
  var
    nr_of_units = 0
    unit_size   = 0
    hash_exists = false
    bitmapstr   = ""
  mfs         = mediafire_session
  echo "[check    ] " & path
  try3times(path):
    if file_size <= 0:
      echo "[*error*] " & path
      echo " *** MF Bug *** zero length files are not supported ***"
    elif file_needs_update(path, file_size_str, file_sig,
                               nr_of_units, unit_size, hash_exists,
                               bitmapstr):
      if hash_exists:
        echo "[quick upl] " & path
        instant_upload(dir_path, name, file_size_str, file_sig)
      else:
        echo "[upload   ] " & path
        resumable_upload(path, dir_path, name, file_size_str,
                             file_sig, nr_of_units, unit_size, bitmapstr)

#########################################################################
## MediaFire Directories
#########################################################################

proc create_mediafire_folder(path, key : string): string {.gcsafe.}

proc create_mediafire_root_folder(path : string): string =
  let (dir_path, name) = splitPath(path)
  if dir_path == "":
    echo "[dir      ] " & path
    mediafire_xml_value("folder_key",
                            "folder/create",
                            ["foldername", name],
                            ["action_on_duplicate", "replace"])
  else:
    create_mediafire_folder(
      path, create_mediafire_root_folder(dir_path))

proc create_mediafire_folder(path, key : string): string =
  if key == "":
    create_mediafire_root_folder( path)
  else:
    echo "[dir      ] " & path
    mediafire_xml_value("folder_key",
                            "folder/create",
                            ["foldername", splitPath(path).tail],
                            ["action_on_duplicate", "replace"],
                            ["parent_key", key])

proc upload_directory(path : string, key : string = "") =
  try3times(path):
    let dir_key = create_mediafire_folder(path, key)
    for kind, path in walkDir(path):
      if kind == pcDir:
        upload_directory(path, dir_key)
      elif kind == pcFile:
        discard mfsid()
        spawnX upload_file(mfs, path)

#########################################################################
## Main
#########################################################################

proc main() =
  signal(SIGPIPE, SIG_IGN)
  setMaxPoolSize(3)
  mfs = (str: "NN", time: -999.99)
  const dirs = ["/backups"]
# const dirs = ["/home/wiffel/kashbah"]
  for dir in dirs:
    upload_directory(dir)

main()

#########################################################################
