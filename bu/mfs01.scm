(use srfi-13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#>

#define FUSE_USE_VERSION 30
#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

static const char *hello_str = "Hello World!\n";
static const char *hello_path = "/hello";

/// Tools ///////////////////////////////////////////////////////////////

static inline void stat_set_st_mode(struct stat *stbuf, int mode)
{
  stbuf->st_mode = mode;
}

static inline void stat_set_st_nlink(struct stat *stbuf, int nlink)
{
  stbuf->st_nlink = nlink;
}

static inline void stat_set_st_size(struct stat *stbuf, int size)
{
  stbuf->st_size = size;
}

static inline void filler_add_name(char *name, void *buf, void* filler)
{
  fuse_fill_dir_t fillr = (fuse_fill_dir_t) filler;
  fillr(buf, name, NULL, 0);
}

/// Fuse Operations /////////////////////////////////////////////////////

static int mfs_c_getattr(const char *path, struct stat *stbuf)
{
  memset(stbuf, 0, sizeof(struct stat));
  return mfs_getattr(path, stbuf);
}

static int mfs_c_readdir(const char *path, void *buf, 
                         fuse_fill_dir_t filler, off_t offset, 
                         struct fuse_file_info *fi)
{
  return mfs_readdir(path, buf, filler);
}

static int mfs_c_open(const char *path, struct fuse_file_info *fi)
{
  if (strcmp(path, hello_path) != 0)
    return -ENOENT;
  if ((fi->flags & 3) != O_RDONLY)
    return -EACCES;
  return 0;
}

static int mfs_c_read(const char *path, char *buf, size_t size, 
                      off_t offset, struct fuse_file_info *fi)
{
  size_t len;

  if(strcmp(path, hello_path) != 0)
    return -ENOENT;
  len = strlen(hello_str);
  if (offset < len) {
    if (offset + size > len)
      size = len - offset;
    memcpy(buf, hello_str + offset, size);
  } else
    size = 0;
  return size;
}

/// Fuse Main ///////////////////////////////////////////////////////////

static struct fuse_operations mfs_c_oper = 
{
  .getattr = mfs_c_getattr,
  .readdir = mfs_c_readdir,
  .open    = mfs_c_open,
  .read    = mfs_c_read,
};

static void run_fuse()
{
  char *argv[] = {"mfs", "-s", "-f", "ttt", NULL};
  int argc = 4;

  fuse_main(argc, argv, &mfs_c_oper, NULL);
}

<#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define S-IFDIR 16384)
(define S-IFREG 32768)
(define E-NOENT -2)

(define stat-set-st-mode
  (foreign-safe-lambda 
    void "stat_set_st_mode" (c-pointer (struct stat)) int))

(define stat-set-st-nlink
  (foreign-safe-lambda 
    void "stat_set_st_nlink" (c-pointer (struct stat)) int))

(define stat-set-st-size
  (foreign-safe-lambda 
    void "stat_set_st_size" (c-pointer (struct stat)) int))

(define filler-add-name
  (foreign-safe-lambda 
    void "filler_add_name" c-string c-pointer c-pointer))

(define run-fuse 
  (foreign-safe-lambda 
    void "run_fuse"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fuse Callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-external (mfs_readdir ((const c-string) str)
                              (c-pointer buf)
                              (c-pointer filler)) int
; (print "mfs_readdir: " str)
  (cond
    [(string= str "/")
       (filler-add-name "." buf filler)
       (filler-add-name ".." buf filler)
       (filler-add-name "hello" buf filler)
       0]
    [else E-NOENT]))

(define-external (mfs_getattr ((const c-string) str)
                              ((c-pointer (struct stat)) stbuf)) int 
; (print "mfs_getattr: " str)
  (cond
    [(string= str "/")
       (stat-set-st-mode stbuf (bitwise-ior S-IFDIR #o755))
       (stat-set-st-nlink stbuf 2)
       0]
    [(string= str "/hello")
       (stat-set-st-mode stbuf (bitwise-ior S-IFREG #o444))
       (stat-set-st-nlink stbuf 1)
       (stat-set-st-size stbuf 13)
       0]
    [else E-NOENT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "run-fuse ->" )
(run-fuse)
(print "run-fuse <-" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

