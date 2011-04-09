! Copyright (C) 2011 Martial Boniou.
! See http://factorcode.org/license.txt for BSD license.
! hack from editors.emacs by Slava Pestov.
USING: definitions io.launcher io.pathnames namespaces kernel
parser words sequences math math.parser namespaces editors make
system combinators.short-circuit fry threads vocabs.loader ;
IN: editors.emacs-mars

SYMBOL: emacsclient-path
SYMBOL: emacsserver-name

HOOK: default-emacsclient os ( -- path )
HOOK: default-emacsserver os ( -- path )

M: object default-emacsclient ( -- path ) "emacsclient" ;
M: object default-emacsserver ( -- path ) "mars"        ;

: emacsclient ( file line -- )
    [
        {
            [ emacsclient-path get-global ]
            [ default-emacsclient dup emacsclient-path set-global ]
        } 0|| ,
        {
            [ emacsserver-name get-global ]
            [ default-emacsserver dup emacsserver-name set-global ]
        } 0|| "--socket-name=" prepend ,
        "--no-wait" ,
        number>string "+" prepend ,
        ,
    ] { } make
    os windows? [ run-detached drop ] [ try-process ] if ;

: emacs ( word -- )
    where first2 emacsclient ;

[ emacsclient ] edit-hook set-global

os windows? [ "editors.emacs-mars.windows" require ] when
