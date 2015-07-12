#!/usr/bin/env python

import os
import sys
import time
import posixpath
import mediafire.uploader
from mediafire import (MediaFireApi, MediaFireUploader)
from mediafire.client import (MediaFireClient, ResourceNotFoundError, 
                              File, Folder)

### Parameters ######################################################

EMAIL    = 'wiffel@tilient.net'
PASSWORD = 'ttT1l1ent'
APP_ID   = '44475'
API_KEY  = 'zrfwexvc04rre7089f9uiq5vo89oxxgtmc9x2v5e'
DIRS     = ["/backups",
            "/home/wiffel/Pictures",
            "/home/wiffel/Music",
            "/home/wiffel/Videos"]

### Tuning Parameters ###############################################

mediafire.uploader.UPLOAD_SIMPLE_LIMIT = 64 * 1024 * 1024
mediafire.uploader.UPLOAD_POLL_INTERVAL = 2
RETRIES  = 10

### Connection ######################################################

api      = None
client   = None
uploader = None

def connect():
  global api
  global client
  global uploader
  api = MediaFireApi()
  api.session = api.user_get_session_token(
                  email = EMAIL, password = PASSWORD,
                  app_id = APP_ID, api_key = API_KEY)
  uploader = MediaFireUploader(api)
  client = MediaFireClient()
  client.login(EMAIL, PASSWORD, 
               app_id = APP_ID, api_key = API_KEY)

def guarded(fun):
  for _ in range(RETRIES):
    try:
      return fun()
    except Exception, e:
      print "*** ERROR *** " + str(e)
      time.sleep(5)
      connect()
      time.sleep(5)
      pass

### Upload ##########################################################

def create_folder(uri):
  global api
  global client
  uri = uri.rstrip('/')
  try:
    node = client.get_resource_by_uri(uri)
    return node["folderkey"]
  except ResourceNotFoundError:
    folder_name = posixpath.basename(uri)
    parent_uri  = posixpath.dirname(uri)
    parent_key  = create_folder(parent_uri)
    result = api.folder_create(
               folder_name, parent_key = parent_key, 
               allow_duplicate_name = 'no')
    return result['folderkey']

def upload_file(dirname, folderkey, filename):
  global uploader
  fd = open(os.path.join(dirname, filename), 'rb')
  guarded(lambda: 
          uploader.upload(fd, filename, folder_key = folderkey))

def upload_dirs(dirs):
  for dir in dirs:
    for dirname, _, filenames in os.walk(dir):
      print "-- DIR -- " + dirname
      folderkey = create_folder(dirname)
      for filename in filenames:
        print filename
        upload_file(dirname, folderkey, filename)

### Download ########################################################

def download_dir(dir, prefix):
    print "-- DIR -- " + dir
    for item in client.get_folder_contents_iter(dir):
      if type(item) is File:
        filename = dir + "/" + item['filename']
        print filename
        guarded(lambda: 
                client.download_file(filename, prefix + filename))
      elif type(item) is Folder:
        download_dir(dir + "/" + item['name'], prefix)

def download_dirs(dirs, prefix):
  for dir in dirs:
    download_dir(dir, prefix)

### Main ############################################################

if __name__ == "__main__":
  connect()
  upload_dirs(DIRS)
# download_dirs(["/backups"], "/tmp/")

#####################################################################
