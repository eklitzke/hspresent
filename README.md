Presenting hspresent
====================

This is a simple program that lets you give powerpoint-like presentations in
your terminal (for certain definitions of powerpoint-like). It's extremely basic
and unintelligent. Don't expect much.

Compile and install using cabal:
    cabal build && cabal install

This will compile hspresent, and install an `hspresent` command line program.

You can download the latest stable release of hspresent on
[Hackage](http://hackage.haskell.org/package/hspresent). The git repository is
online at `git://github.com/eklitzke/hspresent.git`.

Usage
-----
Invoke like `hspresent /path/to/your/presentation`. Left and right arrow keys
move between slides, and hitting `q` or `Ctrl-C` quits the presentation.

The file format is really simple right now. Slides are separated by lines
consisting of the characters `--`. That's it. Here's an example presentation:
    the title of the first slide
    this is
    really cool
    --
    the title of the second slide
    hooray for hspresent
    --
    look at how fancy the title to this slide is
    * bullet point one
    * bullet point two
