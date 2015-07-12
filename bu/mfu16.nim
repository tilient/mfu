#########################################################################
##
## nimble install sha1
##
## nim c -d:release -d:ssl --verbosity:0 -w:off mfu16.nim 
## ./mfu16
##
#########################################################################

import strutils, times, httpclient, os

#########################################################################
## Parameters
#########################################################################

const appid   = "44501"
const apikey  = "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff"
const email   = "wiffel@tilient.net"
const passwd  = "ttT1l1ent"
const dirs    = ["/home/wiffel/kashbah/sss"]

#########################################################################
## libssl - SHA 
#########################################################################

const SHA1Len = 20
 
proc SHA1(d: cstring, n: culong, md: cstring = nil): cstring 
         {.cdecl, dynlib: "libssl.so", importc.}
  
proc SHA1(s: string): string =
  result = ""
  var s = SHA1(s.cstring, s.len.culong)
  for i in 0 .. < SHA1Len:
    result.add s[i].BiggestInt.toHex(2).toLower
                 
#########################################################################

const SHA256Len = 32
                  
proc SHA256(d: cstring, n: culong, md: cstring = nil): cstring
           {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Init (c: ptr): int
                 {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Update (c : ptr, data : cstring, n : culong): int
                   {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256_Final (md : cstring, c : ptr): int
                  {.cdecl, dynlib: "libssl.so", importc.}

proc SHA256(s: string): string =
  result = ""
  let s = SHA256(s.cstring, s.len.culong)
  for i in 0 .. < SHA256Len:
    result.add s[i].BiggestInt.toHex(2).toLower
                                  
proc SHA256_Sum(filename : string): string =
  const sha256_bufsize = 1024
  var f : File
  if f.open(filename):
    let 
      ctxo = alloc(256)
      ctx  = cast[ptr char](ctxo)
      buf  = alloc(sha256_bufsize)
      bufp = cast[pointer](buf)
      bufs = cast[ptr char](buf)
      md   = "1234567890123456789012345678901234567890"
    discard SHA256_Init(ctx)
    while not endOfFile(f):
      let bytesRead = readBuffer(f, bufp, sha256_bufsize)
      discard SHA256_Update(ctx, bufs, bytesRead.culong)
    f.close()
    discard SHA256_Final(md, ctx)
    dealloc(ctxo)
    dealloc(buf)
    result = ""
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
    "Not Found"

proc xml_value(str, key : string): string =
  str.string_between("<" & key & ">", "</" & key & ">")

proc xml_int_value(str, key : string): int =
  parseInt(xml_value(str, key))

proc xml_value_equals(str, key, value : string): bool =
  value == xml_value(str, key)

#########################################################################
## MediaFire API
#########################################################################

proc session(): string

let signature = SHA1(email & passwd & appid & apikey)

proc mediafire_call(op : string,
                    params : varargs[array[2,string]]): string =
  var url = "https://www.mediafire.com/api/1.3/" & op &
            ".php?token_version=1&session_token=" & session()
  for param in items(params):
    let key   = param[0]
    let value = param[1]
    url &= "&" & key & "=" & value
# echo "=== url ================================="
# echo url
# echo "---- res --------------------------------"
  let res = getContent(url)
# echo res
# echo "========================================="
  res

proc mediafire_xml_value(key : string, op : string,
                         params : varargs[array[2,string]]): string =
  mediafire_call(op, params).xml_value(key)

#########################################################################
## MediaFire Session
#########################################################################

var session_str = "NN"
var session_time = -999.99

proc session(): string =
  let
    now = cpuTime()
    age = now - session_time
  if age > 400:
    session_time = now
    session_str = mediafire_xml_value("session_token",
                                      "user/get_session_token",
                                      ["signature", signature],
                                      ["email", email],
                                      ["password", passwd],
                                      ["application_key", apikey],
                                      ["application_id", appid])
  session_str

#########################################################################
## Main
#########################################################################

proc file_needs_update(file_path, file_size, file_sig : string,
                       nr_of_units, unit_size: var int): bool =
  let 
    paths = splitPath(file_path)
    dir_path = paths.head
    filename = paths.tail
    res = mediafire_call("upload/check",
                           ["filename", filename],
                           ["resumable", "yes"],
                           ["size", file_size],
                           ["hash", file_sig],
                           ["path", dir_path])
    file_exists = res.xml_value_equals("file_exists", "yes")
    same_hash = res.xml_value_equals("different_hash", "no")
  nr_of_units = res.xml_int_value("number_of_units")
  unit_size= res.xml_int_value("unit_size")
  not (file_exists and same_hash)

proc create_mediafire_root_folder(path : string): string

proc create_mediafire_folder(path; key : string): string =
  if key == "":
    create_mediafire_root_folder(path)
  else:
    echo "[dir   ] " & path
    mediafire_xml_value("folder_key",
                        "folder/create",
                        ["foldername", splitPath(path).tail],
                        ["action_on_duplicate","replace"],
                        ["parent_key", key])

proc create_mediafire_root_folder(path : string): string =
  let 
    paths = splitPath(path)
    dir_path = paths.head
    name = paths.tail
  if dir_path == "":
    echo "[dir  *] " & path
    mediafire_xml_value("folder_key",
                        "folder/create",
                        ["foldername", name],
                        ["action_on_duplicate","replace"])
  else:
    create_mediafire_folder(path, create_mediafire_root_folder(dir_path))

proc upload_file(path : string) =
  let 
    paths    = splitPath(path)
    dir_path = paths.head
    name     = paths.tail
    filesize = getFileSize(path)
    sig      = SHA256_Sum(path)
  var 
    nr_of_units = 0
    unit_size   = 0
  echo "[check ] " & path
  if filesize <= 0:
    echo "  *** MediaFire Bug *** "
    return
  if file_needs_update(path, $filesize, sig, nr_of_units, unit_size):
    var f : File
    if f.open(path):
      var buf {.noinit.}: array[4444444, char]
      var data = newMultipartData()
      for ix in 0 .. < nr_of_units:
        echo "[upload] (" & $(1+ix) & "/" & $nr_of_units & ") " & path
        let bytesRead = readChars(f, buf, 0, unit_size)
        var str = newStringOfCap(bytesRead * 2)
        for i in 0 .. < bytesRead:
          str.add(buf[i])
        data["file"] = (name, "text/html", str)
        let
          unit_hash = SHA256(str)
          extra_headers = "x-unit-hash:" & unit_hash  & "\c\L" &
                          "x-filehash:"  & sig        & "\c\L" &
                          "x-filesize:"  & $filesize  & "\c\L" &
                          "x-unit-id:"   & $ix        & "\c\L" &
                          "x-unit-size:" & $bytesRead
          url = "https://www.mediafire.com/api/1.3/" &
                "upload/resumable.php?token_version=1" &
                "&session_token=" & session() &
                "&resumable=yes&action_on_duplicate=replace" &
                "&path=" & dir_path 
#       echo "=== ================================="
#       echo url
#       echo "--- ---------------------------------"
#       echo extra_headers
#       echo "--- ---------------------------------"
        discard postContent(url, extra_headers, multipart = data)
#       echo "=== ================================="

      f.close()

proc upload_directory(path : string, key : string = "") =
  let dir_key = create_mediafire_folder(path, key)
  for kind, path in walkDir(path):
    if kind == pcDir:
      upload_directory(path, dir_key)
    elif kind == pcFile:
      upload_file(path)
    else:
      echo "*** Links are not supported ***"


#########################################################################

for dir in dirs:
  upload_directory(dir)

#########################################################################
