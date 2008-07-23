;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/util.lisp,v 1.26 2008/07/22 12:20:14 edi Exp $

;;; Copyright (c) 2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-unicode)

(defun parse-hex (string)
  "Parses STRING as a hexadecimal number."
  (parse-integer string :radix 16))

(defun canonicalize-name (name)
  "Converts the string NAME into a \"canonicalized\" name which can be
used for unambiguous look-ups by removing all whitespace, hyphens, and
underline characters.

Tries not to remove hyphens preceded by spaces if this could lead to
ambiguities as described in
<http://unicode.org/unicode/reports/tr18/#Name_Properties>.

All CL-UNICODE functions which accept string \"names\" for characters
or properties will canonicalize the name first using this function and
will then look up the name case-insensitively."
  (values (ppcre:regex-replace-all "( -A| O-E)$|[-_\\s]" name
                                   (lambda (match register)
                                     (declare (ignore match))
                                     (or register ""))
                                   :simple-calls t)))

(defun property-symbol (name)
  "Returns a symbol in the CL-UNICODE-NAMES packages \(which is only
used for this purpose) which can stand in for the string NAME in
look-ups.  The symbol's name is the result of \"canonicalizing\" and
then upcasing NAME.

A symbol returned by this function is only really useful and only
actually a property symbol if the second return value is true.

All exported functions of CL-UNICODE which return strings which are
property names return the corresponding property symbol as their
second return value.  All exported functions of CL-UNICODE which
accept property names as arguments will also accept property symbols.

See also PROPERTY-NAME."
  (let ((symbol (intern (string-upcase (canonicalize-name name)) :cl-unicode-names)))
    (values symbol (property-name symbol))))

(defun register-property-symbol (name)
  "Converts NAME to a property symbol using PROPERTY-SYMBOL and
\"registers\" it in the *CANONICAL-NAMES* hash table."
  (let ((symbol (property-symbol name)))
    (setf (gethash symbol *canonical-names*) name)
    symbol))

(defun property-name (symbol)
  "Returns a name \(not \"the\" name) for a property symbol SYMBOL if
it is known to CL-UNICODE.  Note that

  \(STRING= \(PROPERTY-NAME \(PROPERTY-SYMBOL <string>)) <string>)

is not necessarily true even if the property name is not NIL while

  \(EQ \(PROPERTY-SYMBOL \(PROPERTY-NAME <symbol>)) <symbol>)

always holds if there is a property name for <symbol>.

See also PROPERTY-SYMBOL."
  (values (gethash symbol *canonical-names*)))

(defun tree-lookup (code-point tree)
  "Looks up an attribute for CODE-POINT in the binary search tree
TREE.  TREE is a tree as created by BUILD-TREE."
  (labels ((try (node)
             (and node
                  (destructuring-bind (((from . to) . value) left-branch right-branch)
                      node
                    (cond ((< code-point from) (try left-branch))
                          ((> code-point to) (try right-branch))
                          (t value))))))
    (try tree)))

(defun try-abbreviations (name scripts-to-try)
  "Helper function called by CHARACTER-NAMED when the
:TRY-ABBREVIATIONS-P keyword argument is true.  Tries to interpret
NAME as an abbreviation for a longer Unicode name and returns the
corresponding code point if it succeeds."
  (flet ((size-word (string)
           (if (ppcre:scan "[A-Z]" string) "CAPITAL" "SMALL"))
         (try (script size-word short-name)
           (or (character-named (format nil "~A ~A letter ~A"
                                        script size-word short-name)
                                :want-code-point-p t)
               (character-named (format nil "~A letter ~A"
                                        script short-name)
                                :want-code-point-p t)
               (character-named (format nil "~A ~A"
                                        script short-name)
                                :want-code-point-p t))))
    (ppcre:register-groups-bind (script short-name)
        ("^([^:]+):([^:]+)$" name)
      (let ((size-word (size-word short-name)))
        (return-from try-abbreviations
          (try script size-word short-name))))
    (loop with size-word = (size-word name)
          for script in scripts-to-try
          thereis (try script size-word name))))  

(defgeneric mapping (c position want-code-point-p)
  (:documentation "Returns the simple case mapping for the character C
\(a code point or a Lisp character) in position POSITION where 0 means
lowercase, 1 uppercase, and 2 titlecase.  Returns a character if
WANT-CODE-POINT-P is NIL and a code point otherwise.")
  (:method ((char character) position want-code-point-p)
   (mapping (char-code char) position want-code-point-p))
  (:method ((code-point integer) position want-code-point-p)
   (let* ((mappings (gethash code-point *case-mappings*))
          (code-point (or (nth position mappings) code-point)))
     (if want-code-point-p
       code-point
       (and code-point (code-char code-point))))))

(defun cjk-unified-ideograph-p (code-point)
  "Returns a true value if CODE-POINT is the code point of a CJK
unified ideograph for which we can algorithmically derive the name."
  (or (<= #x3400 code-point #x4db5)
      (<= #x4e00 code-point #x9fc3)
      (<= #x20000 code-point #x2a6d6)))

(defun maybe-compute-cjk-name (code-point)
  "Computes the name for CODE-POINT if CODE-POINT denotes a CJK
unified ideograph the name of which can be algorithmically derived."
  (when (cjk-unified-ideograph-p code-point)
    (format nil "CJK UNIFIED IDEOGRAPH-~X" code-point)))

(defun maybe-find-cjk-code-point (name)
  "Computes the code point for NAME if NAME is the name of a CJK
unified ideograph the name of which can be algorithmically derived."
  (ppcre:register-groups-bind ((#'parse-hex code-point))
      ;; canonicalized
      ("(?i)^CJKUNIFIEDIDEOGRAPH([0-9A-F]{4,5}|10[0-9A-F]{4})$" name)
    (when (cjk-unified-ideograph-p code-point)
      code-point)))

(defconstant +s-base+ #xac00
  "The constant `SBase' from chapter 3 of the Unicode book.")
(defconstant +l-base+ #x1100
  "The constant `LBase' from chapter 3 of the Unicode book.")
(defconstant +v-base+ #x1161
  "The constant `VBase' from chapter 3 of the Unicode book.")
(defconstant +t-base+ #x11a7
  "The constant `TBase' from chapter 3 of the Unicode book.")
(defconstant +v-count+ 21
  "The constant `VCount' from chapter 3 of the Unicode book.")
(defconstant +t-count+ 28
  "The constant `TCount' from chapter 3 of the Unicode book.")
(define-symbol-macro +n-count+
  ;; the constant `NCount' from chapter 3 of the Unicode book
  (* +v-count+ +t-count+))

(defun compute-hangul-name (code-point)
  "Algorithmically derives the Hangul syllable name of the character
with code point CODE-POINT as described in section 3.12 of the Unicode
book."
  (let* ((s-index (- code-point +s-base+))
         (l-value (+ +l-base+ (floor s-index +n-count+)))
         (v-value (+ +v-base+ (floor (mod s-index +n-count+) +t-count+)))
         (t-value (+ +t-base+ (mod s-index +t-count+))))
    (format nil "HANGUL SYLLABLE ~A~A~@[~A~]"
            (gethash l-value *jamo-short-names*)
            (gethash v-value *jamo-short-names*)
            (and (/= t-value +t-base+)
                 (gethash t-value *jamo-short-names*)))))

(defun add-hangul-names ()
  "Computes the names for all Hangul syllables and registers them in
the appropriate hash tables."
  (format t "~&;;; Computing Hangul syllable names")
  (loop for code-point from #xac00 to #xd7a3
        for name = (compute-hangul-name code-point)
        do (setf (gethash (canonicalize-name name) *names-to-code-points*) code-point
                 (gethash code-point *code-points-to-names*) name)))

(defmacro ensure-code-point (c)
  "Helper macro so that C can be treated like a code point even if it
is a Lisp character."
  (with-rebinding (c)
    `(etypecase ,c
       (integer ,c)
       (character (char-code ,c)))))

(defun unicode-name-reader (stream char arg)
  "The reader functino used when the alternative character syntax is
enabled."
  (declare (ignore char arg))
  (let ((name (with-output-to-string (out)
                (write-char (read-char stream t nil t) out)
                (loop for next-char = (read-char stream t nil t)
                      while (find next-char "abcdefghijklmnopqrstuvwxyz0123456789_-+:"
                                  :test 'char-equal)
                      do (write-char next-char out)
                      finally (unread-char next-char stream)))))
    (or (character-named name)
        (error 'character-not-found :name name))))

(defun %enable-alternative-character-syntax ()
  "Internal function used to enable alternative character syntax and
store current readtable on stack."
  (push *readtable* *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\\ #'unicode-name-reader)
  (values))

(defun %disable-alternative-character-syntax ()
  "Internal function used to restore previous readtable." 
  (setq *readtable*
        (if *previous-readtables*
          (pop *previous-readtables*)
          (copy-readtable nil)))
  (values))