# Dataloader - A universal loader library for various data formats for images/audio

This is what you might have needed for a long time.  It loads a file in an arbitrary
data format into a [numcl-compatible array](https://github.com/numcl/numcl/blob/master/doc/DETAILS.org#representation).

## Usage

``` common-lisp
load (file &rest args
         &key (mime (magicffi:pathname-mime-type file))
         ;; asarray
           type
           ;; csv args
           csv-reader row-fn map-fn data-map-fn sample skip-first-p 
           (separator                    cl-csv:*separator*)
           (quote                        cl-csv:*quote*)
           (escape                       cl-csv:*quote-escape*)
           unquoted-empty-string-is-nil
           quoted-empty-string-is-nil
           trim-outer-whitespace
           newline
           escape-mode
         &allow-other-keys)
```

Load an arbitrary file using the mime type information obtained by `libmagic`.

Supported files are currently `png`, `jpg`, `tiff`, `bmp`, `csv`, `tsv`.
We plan to include support for `wav`, `gif`, `npy`, `npz`.

This library merely provides a bridge to various libraries. It uses `cl-png`,
`retrospectiff`, `cl-csv` and so on.

``` common-lisp
save (array file &key (mime (pathname-type file)))
```

This function saves the array into a file, using the file name extension or the
additional `mime` argument.

## Installation

You currently need a patched version of `magicffi` in https://github.com/guicho271828/magicffi .
It also depends on [NUMCL](https://github.com/numcl/numcl).

If you have `roswell`, run follows:

    ros install numcl/constantfold numcl/specialized-function numcl/gtype numcl/numcl guicho271828/magicffi guicho271828/dataloader

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
