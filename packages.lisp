(in-package :cl-user)

(defpackage :me.ryuki.macro-utilities
  (:use :common-lisp)
  (:export 
   :with-gensyms
   :with-gensymed-defuns
   :once-only
   :spliceable
   :ppme))

(defpackage :me.ryuki.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

(defpackage :me.ryuki.binary-data
  (:use :common-lisp :me.ryuki.macro-utilities)
  (:export :define-binary-class
   :define-tagged-binary-class
           :define-binary-type
   :read-value
           :write-value
   :*in-progress-objects*
           :parent-of-type
   :current-binary-object
           :+null+))

(defpackage :me.ryuki.id3v2
  (:use :common-lisp
        :me.ryuki.binary-data
        :me.ryuki.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))

(defpackage :me.ryuki.html
  (:use :common-lisp :me.ryuki.macro-utilities)
  (:export :with-html-output
           :with-html-to-file
           :in-html-style
           :define-html-macro
           :define-css-macro
           :css
           :html
           :emit-css
           :emit-html
           :&attributes))

(defpackage :me.ryuki.url-function
  (:use :common-lisp 
   :net.aserve 
        :me.ryuki.html
   :me.ryuki.macro-utilities)
  (:export :define-url-function
   :string->type))

(defpackage :me.ryuki.mp3-database
  (:use :common-lisp 
        :me.ryuki.pathnames
        :me.ryuki.macro-utilities
        :me.ryuki.id3v2)
  (:export  :*default-table-size*
            :*mp3-schema*
            :*mp3s*
            :column
            :column-value
            :delete-all-rows
            :delete-rows
            :do-rows
            :extract-schema
            :in
            :insert-row
            :load-database
            :make-column
            :make-schema
            :map-rows
            :matching
            :not-nullable
            :nth-row
            :random-selection
            :schema
            :select
            :shuffle-table
            :sort-rows
            :table
            :table-size
            :with-column-values))

(defpackage :me.ryuki.shoutcast
  (:use :common-lisp 
        :net.aserve 
        :me.ryuki.id3v2)
  (:export :song
           :file
           :title
           :id3-size
           :find-song-source
           :current-song
           :still-current-p
           :maybe-move-to-next-song
           :*song-source-type*))

(defpackage :me.ryuki.mp3-browser
  (:use :common-lisp 
        :net.aserve 
        :me.ryuki.html
        :me.ryuki.shoutcast
        :me.ryuki.url-function
        :me.ryuki.mp3-database
        :me.ryuki.id3v2)
  (:import-from :acl-socket
                :ipaddr-to-dotted 
                :remote-host)
  (:import-from #+allegro :multiprocessing 
                #-allegro :acl-compat.mp
                :make-process-lock
                :with-process-lock)
  (:export :start-mp3-browser))
