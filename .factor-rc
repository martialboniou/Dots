USING: kernel namespaces parser io.pathnames vocabs.loader sequences tools.scaffold ;
IN: scratchpad
: append-home ( str -- str )
    home swap append-path ;
".factor.d/utils" append-home add-vocab-root
"Martial Boniou" \ developer-name set-global

! run the edit hack
".emacs.factor" append-home ?run-file



