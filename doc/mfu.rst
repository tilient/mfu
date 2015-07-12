
#########################################################################
mfu : MediaFire Uploader
#########################################################################


=========================================================================
Installation
=========================================================================

**mfu** uses two libraries: **libCurl** and **libSsl**. We need to
make sure that these are installed first.

libCurl
-------

If it's not already installed, you can - e.g. in Ubuntu - install
**libCurl** using the follow command in the shell:

.. code:: bash

   sudo apt-get install libcurl3

libSsl
------

Make sure **libssl.so** is available in the library path.
On Ubuntu I had to make a link to the library to make it available:

.. code:: bash

   sudo ln -s /lib/x86_64-linux-gnu/libssl.so.1.0.0 /usr/lib/libssl.so

mfu
---

Copy ``mfu`` somewhere in the path, so it can be executed, e.g:

.. code:: bash

    cp mfu \usr\bin\


=========================================================================
Usage
=========================================================================

You can run **mfu** as-is, without any parameters.
But you need to create a configuration file first (see below).

.. code:: bash

    mfu

=========================================================================
MediaFire Account
=========================================================================

**mfu** uses *MediaFire*, so you need a MediaFire Account.
From that account we need the *username* and *password*, e.g.:

- **Username** : *gniffel*
- **Password** : *5i2a1o*

You also need to enable the *API* access to your files for **mfu**.
For that, it needs an *App id* and an *API Key*.

You can create these in your MediaFire configuration page on the web:
**Account Settings** >> **Developers** >> **Create New Application**

This will give us the *App ID* and the *API Key*, e.g.:

- **App ID**  : *47581*
- **API Key** : *ei96jj7lyxoaesfj61an33wtwxj7denpddo2a4ff*


=========================================================================
Profiles
=========================================================================

**mfu** uses *profiles*. A *profile* contains the information about
which directories need to be uploaded, on which MediaFire account,
using how many upload threads, the password, ...
It also holds some information about the files and directories
that are already uploaded.
You can have multiple profiles. If you only need one, just use the
*main* profile.

*Profiles* are stored in directory ``~/.mfu``.
The default profile is **main**.
So, the default profile directory is ``~/.mfu/main``.
In each profile directory, a ``config`` file can be placed which
contains some parameter values (see below).
The profile directory is also used to store cache files, which contains
cached information about files and directories that where backuped.


=========================================================================
Command Line
=========================================================================

**mfu** takes its parameter from the profile configuration file and
from the command line. The first parameter is the name of the
*profile*. If that is ommitted, it takes *main* as the profile.

e.g.: ``mfu`` : uses the default profile *main* and reads the parameters
from ``~\.mfu\main\config``

e.g.: ``mfu profilename`` : uses the profile *profilename* and reads the
parameters from ``~\.mfu\profilename\config``

It is possible to have parameters on the *command line*. These take
the form ``--parameter:value``. Parameters on the *command line*
override the ones in the configuration file.

e.g.
``mfu profilename --loglevel:6`` : uses the profile *profilename* but
uses a different *loglevel*.

=========================================================================
Supported Parameters
=========================================================================

Parameters can be put in the *config* file or on the command line.
**mfu** first reads the *config* file, then the command line parameters.
So, command line parameter override the ones in the *config* file.

An exception to this is the **dir** parameter. A *dir* parameter on the
command line is treated the same as the multiple *dir* parameters in the
config file: it will be added to the list of directories to be uploaded.

========================== ====================================
parameter                  description
========================== ====================================
email, e-mail              the MediaFire email for your account

passwd, password           the password for your
                           MediaFire account

appid, app-id              the AppID for the application access
                           on your MediaFire account

apikey, api-key            the AppKey for the application
                           access on your MediaFire account

dir, directory             A directory that needs to be
                           uploaded. This can occur
                           multiple times to upload multiple
                           directories.

quick                      *on* of *off*
                           See description below

uploaders, nr-of-uploaders **mfu** uses multiple parallell
                           threads to upload files.
                           By default it uses 4 upload threads.
                           You can change this (up to 16
                           threads) using this parameter.

loglevel, log-level        Controls the amount of information
                           shown by **mfu**. It is a number
                           between 0 and 9. See description
                           below.
========================== ====================================


=========================================================================
quick : ``on`` or ``off``
=========================================================================

In default mode, **mfu** scans and checks all directories and files.
This includes a check against the MediaFire server to see if a file was
changed (deleted, replaced, renamed, ...) on the server.  Also
directories are checked against the server to see if they still exists.
This makes the uploads very safe, but it can take a lot of time if there
are lots of files and directories.

If you are sure that nothing has changed on the MediaFire server since
the last upload - e.g. because **mfu** is the only application touching
these files - it is safe to enable the *quick* mode. In this case,
**mfu** will check the local files and directories against the caches
(stored in the *profile* folder) to see if they need to be uploaded.
This makes incremental uploads *a lot* faster.


=========================================================================
loglevel or log-level
=========================================================================

With this parameter the amount of progress  information that is shown
can be controlled.

========== ====================================================
 loglevel  description
========== ====================================================
 0         nothing is shown
 1         errors are shown
 2         uploaded files are shown
           (default)
 3         directories that are being created/checked are shown
           incremental uploads of big files are shown
 9         extensive debug information is shown
========== ====================================================


=========================================================================
Examples
=========================================================================

the  *config* file I use to upload my backups

.. code::

    >> cat ~/.mfu/main/config

    appid     : 47581
    apikey    : ei96jj7lyxoaesfj61an33wtwxj7denpddo2a4ff
    email     : name@testing.net
    passwd    : p@ssw0rd
    uploaders : 8
    loglevel  : 3
    quick     : on
    dir       : /backups


the *config* file I use to upload my media files

.. code::

    >> cat ~/.mfu/media/config

    appid     : 47581
    apikey    : ei96jj7lyxoaesfj61an33wtwxj7denpddo2a4ff
    email     : name@testing.net
    passwd    : p@ssw0rd
    quick     : on
    dir       : /home/user/Music
    dir       : /home/user/Videoss
    dir       : /home/user/Pictures


=========================================================================
Build Instructions
=========================================================================

**mfu** is written in **nim**.
To build **mfu** yourself, you first need to instal **nim**.
(see `nim-lang.org <http://nim-lang.org/>`_)

You can find installation instructions at:
`nim-lang.org/download <http://nim-lang.org/download.html>`_

**mfu** uses some of the latest features from **nim** that
only reside in de development branch (for the moment).
So you need to use the *devel* branch to build it.

In short ...

.. code:: bash

  git clone -b devel git://github.com/Araq/Nim.git
  cd Nim
  git clone -b devel --depth 1 git://github.com/nim-lang/csources
  cd csources && sh build.sh
  cd ..
  bin/nim c koch
  ./koch boot -d:release

  sudo ./koch install /usr/bin

All of the *mfu* source code resides in one file: *mfu.nim*.
Once **nim** is installed, you can compile *mfu.nim*
into an executable using:

.. code:: bash

  nim c -d:release --threads:on mfu.nim

If you want to make a very tiny executable, you can optionally run:

.. code:: bash

  strip mfu
  upx -q --best mfu


