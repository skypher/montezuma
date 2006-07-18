Copyright 2006 John Wiseman <jjwiseman@yahoo.com>
7/13/2006

** Introduction

Montezuma is a text search engine library for Lisp based on the Ferret
library[1] for Ruby, which is itself based on the Lucene library[2]
for Java.

[1] http://ferret.davebalmain.com/trac
[2] http://lucene.apache.org/


** Requirements

Montezuma has been tested with SBCL 0.9.12 (OS X/PPC), SBCL 0.9.13
(Linux/x86) OpenMCL 1.0 (OS X/PPC) and ACL 8.0 (OS X/PPC).

Montezuma requires the CL-PPCRE[1] and CL-FAD[2] libraries.

The only implementation-dependent code in Montezuma is in
src/util/mop.lisp.  To add support for another implementation may be
as simple as adding one line to the definition of the CLASS-SLOTS
function and one to SLOT-DEFINITION-NAME.

[1] http://www.cliki.net/CL-PPCRE
[2] http://www.cliki.net/CL-FAD


** Installation and Loading

You can use ASDF-INSTALL to install Montezuma:

  (asdf-install:install '#:montezuma)

And ASDF to load it:

  (asdf:oos 'asdf:load-op '#:montezuma)


** Testing

Once Montezuma has been loaded, you can run the unit tests if you
like:

  (asdf:oos 'asdf:test-op '#:montezuma)


** Use

See the TUTORIAL.TXT file for more information on how to use
Montezuma.

The Montezuma project page at http://projects.heavymeta.org/montezuma/
should have the latest information about Montezuma.


** Acknowledgements

Thanks to Dave Balmain, Gary King, Peter Seibel (for his META-inspired
parser), Xach Beane (for the heap implementation from his TIMER
library[1]) and Franz. Inc. (for their Porter stemmer[2]).

[2] http://www.xach.com/lisp/timer/doc.html
[3] http://www.lispwire.com/entry-text-porter-word-stemmer-des
