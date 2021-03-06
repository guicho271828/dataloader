# Dataloader - A universal loader library for various data formats for images/audio [![img](https://travis-ci.org/guicho271828/dataloader.svg)](https://travis-ci.org/guicho271828/dataloader)

This is what you might have needed for a long time.  It loads a file in an arbitrary
data format into a [numcl-compatible array](https://github.com/numcl/numcl/blob/master/doc/DETAILS.org#representation).
File types are automatically detected by the mime type information obtained by `libmagic`.

Supported files are currently `png`, `jpg`, `tiff`, `csv`, `tsv`, `wav`, `npy`.
We plan to include support for `bmp`, `gif`, `npz`.
This library relies on existing libraries to load the files, such as `cl-png`,
`retrospectiff`, `cl-csv`, but provides a consistent and natural array-based interface.

An image file is loaded into a NUMCL-compatible array with corresponding width, height and channels.

A sound file is loaded into a 1D array (monoral), or a 2D array with 2 channels (stereo).
It provides a wrapper over `cl-wav` and correctly returns a `(unsigned-byte 16)` arrays
for a 16-bit PCM file, which is not performed by `cl-wav`.
The save method also automatically generates RIFF headers, which is not performed by `cl-wav`.

A numpy file is loaded with the corresponding width and height using `numpy-file-format` library.

## Load

``` common-lisp
DATALOADER:LOAD
  [symbol]

LOAD names a compiled function:
  Lambda-list: (FILE &REST ARGS &KEY
                (MIME (MAGICFFI:PATHNAME-MIME-TYPE FILE))
                
                ;; jpeg options
                (DECODE-FRAME T) CACHED-SOURCE-P (COLORSPACE-CONVERSION T) BUFFER
                
                ;; csv options
                ESCAPE-MODE NEWLINE
                TRIM-OUTER-WHITESPACE QUOTED-EMPTY-STRING-IS-NIL
                UNQUOTED-EMPTY-STRING-IS-NIL
                (ESCAPE CL-CSV:*QUOTE-ESCAPE*) 'CL-CSV:*QUOTE*
                (SEPARATOR CL-CSV:*SEPARATOR*) SKIP-FIRST-P SAMPLE
                DATA-MAP-FN MAP-FN ROW-FN CSV-READER TYPE)
```

This function loads a file and returns a numcl-compatible array.
The file format can be specified manually in the `mime` string, but by default detected by libmagic.

## Save

``` common-lisp
DATALOADER:SAVE
  [symbol]

SAVE names a compiled function:
  Lambda-list: (ARRAY FILE &REST ARGS &KEY (MIME (PATHNAME-TYPE FILE))
  
                ;; wav options
                (BITS-PER-SECOND 44100))
```

This function saves the array into a file, using the file name extension or the additional `mime` argument.

## Installation

Available from quicklisp.

## Dependencies

This library is at least tested on implementation listed below:

+ SBCL 1.4.12 on X86-64 Linux 4.4.0-141-generic (author's environment)

Also, it depends on the following libraries:

+ cl-wav :
    
+ cl-png :
    
+ cl-jpeg :
    
+ retrospectiff :
    
+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility
+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.
+ trivia by *Masataro Asai* :
    NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.
