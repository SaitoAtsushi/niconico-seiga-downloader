#!/usr/bin/env gosh
;; -*- coding: utf-8; mode: gauche; -*-
;; Author: SAITO Atsushi

;;: set your account
(define-constant *mail* "your e-mail address")
(define-constant *password* "your password")

(define-constant *fsencode*  ;; file-system encoding
  (cond-expand (gauche.os.windows 'Shift_JIS)
               (else 'utf8)))

(use rfc.http)
(use sxml.ssax)
(use sxml.sxpath)
(use srfi-19)
(use rfc.zlib)
(use binary.pack)
(use gauche.parseopt)
(use gauche.sequence)
(use gauche.charconv)
(use gauche.uvector)

(define fsencode
  (cut ces-convert <> (gauche-character-encoding) *fsencode*))

(define path-cleanup (cut regexp-replace-all #/[\\\/;:\t*?\"<>\|]/ <> "_"))

(define-class <nico> ()
  ((user-session)
   (history)))

(define (abstruct-cookie header reg)
  (rxmatch-substring
   (any (compose reg cadr)
        (filter ($ string=? "set-cookie" $ car $) header))))

(define-method nico-login ((self <nico>) mail password)
  (receive (status header body)
      (http-post "secure.nicovideo.jp" "/secure/login?site=niconico"
                 `(("mail" ,mail) ("password" ,password))
                 :secure #t
                 :no-redirect #t)
    (unless
        (string=? "http://www.nicovideo.jp/"
                  (car (assoc-ref header "location")))
      (error "Fail login. Please, check *mail* and *password*."))
    (slot-set! self 'user-session
               (abstruct-cookie header #/user_session=user_session_[^;]+/))
    status))

(define-method get-session ((self <nico>) (id <string>))
  (receive (status header body)
      (http-get "seiga.nicovideo.jp"
                #`"/watch/mg,|id|"
                :cookie (~ self 'user-session))
    status))

(define-method get-data ((self <nico>) (id <string>))
  (receive (status header body)
      (http-get "seiga.nicovideo.jp" `("/api/theme/data" ("theme_id" ,id))
                :cookie (~ self 'user-session))
    (ssax:xml->sxml (open-input-string body) '())))

(define-method get-info ((self <nico>) (id <string>))
  (receive (status header body)
      (http-get "seiga.nicovideo.jp" `("/api/theme/info" ("id" ,id))
                :cookie (~ self 'user-session))
    (ssax:xml->sxml (open-input-string body) '())))

(define query-flag (if-car-sxpath "/response/theme/player_image_id/text()"))

(define query (sxpath "/response/image_list/image/source_url/text()"))

(define (date->dos-format date)
  (let* ((year (date-year date))
         (month (date-month date))
         (day (date-day date))
         (hour (date-hour date))
         (minute (date-minute date))
         (second (date-second date)))
    (+ (ash (- year 1980) 25) (ash month 21) (ash day 16)
       (ash hour 11) (ash minute 5) (quotient second 2))))

(define-class <local-file-header> ()
  ((compress-method :init-keyword :compress-method)
   (timestamp :init-keyword :timestamp)
   (checksum :init-keyword :checksum)
   (compressed-size :init-keyword :compressed-size)
   (uncompressed-size :init-keyword :uncompressed-size)
   (filename-size :init-keyword :filename-size)
   (offset :init-keyword :offset)
   (filename :init-keyword :filename)))

(define-class <zip-archive> ()
  ((port :init-keyword :port)
   (timestamp :init-form (current-date))
   (local-file-headers :init-form '())))

(define-method write-pk0304 ((za <zip-archive>) (lfh <local-file-header>))
  (pack "VvvvVVVVvva*"
    (list #x04034b50
          20
          0
          (~ lfh 'compress-method)
          (date->dos-format (~ lfh 'timestamp))
          (~ lfh 'checksum)
          (~ lfh 'compressed-size)
          (~ lfh 'uncompressed-size)
          (~ lfh 'filename-size)
          0
          (~ lfh 'filename))
    :output (~ za 'port)))

(define (open-output-zip-archive filename)
  (make <zip-archive> :port (open-output-file filename)))

(define-method zip-add-file
  ((za <zip-archive>) (name <string>) (content <string>)
   :key (timestamp (~ za 'timestamp))
   (compression-level Z_DEFAULT_COMPRESSION))
  (let* ((position (port-tell (~ za 'port)))
         (compress-method (if (= compression-level Z_NO_COMPRESSION) 0 8))
         (compressed
          (if (= compress-method 0)
              content
              (deflate-string content
                :window-bits -15
                :compression-level compression-level)))
         (local-file-header
          (make <local-file-header>
            :compress-method compress-method
            :timestamp timestamp
            :checksum (crc32 content)
            :compressed-size (string-size compressed)
            :uncompressed-size (string-size content)
            :filename-size (string-size name)
            :offset position
            :filename name)))
    (write-pk0304 za local-file-header)
    (display compressed (~ za 'port))
    (push! (~ za 'local-file-headers) local-file-header)))

(define-method write-pk0102 ((za <zip-archive>) (lfh <local-file-header>))
  (pack "VvvvvVVVVvvvvvVVa*"
    (list #x02014b50 20 20 0
          (~ lfh 'compress-method)
          (date->dos-format (~ lfh 'timestamp))
          (~ lfh 'checksum)
          (~ lfh 'compressed-size)
          (~ lfh 'uncompressed-size)
          (~ lfh 'filename-size)
          0 0 0 0 0
          (~ lfh 'offset)
          (~ lfh 'filename))
    :output (~ za 'port)))

(define-method zip-close ((za <zip-archive>))
  (let ((cd (port-tell (~ za 'port)))
        (num (length (~ za 'local-file-headers))))
    (for-each (pa$ write-pk0102 za) (reverse (~ za 'local-file-headers)))
    (let1 eoc (port-tell (~ za 'port))
      (pack "VvvvvVVv"
        (list #x06054b50 0 0 num num (- eoc cd) cd 0)
        :output (~ za 'port)))
    (close-output-port (~ za 'port))))

(define query-title (sxpath "/response/theme/title/text()"))

(define (url-split url)
  (let1 m (#/^http:\/\/(.+?)(\/[^?]+)(?:\?(.*))?$/ url)
    (cons (m 1) (m 2))))

(define (hexify2string h)
  (call-with-string-io h
    (lambda(in out)
      (while (read-block 2 in) (complement eof-object?) => x
             (write-byte (read-from-string #`"#x,|x|") out)))))

(define (decrypt data key)
  (let1 key (string->u8vector (hexify2string key))
    (call-with-string-io data
      (lambda(in out)
        (do ((ch (read-byte in) (read-byte in))
             (i 0 (modulo (+ i 1) 8)))
            ((eof-object? ch))
            (write-byte (logxor (~ key i) ch) out))))))

(define (get-key-from-url url)
  (let1 m (#/\/image\/([0-9a-z]+)/ url)
    (m 1)))

(define (path-convert path)
  (regexp-replace #/l$/ path "p"))

(define-method download-seiga ((self <nico>) (id <string>))
  (let1 nico-obj (make <nico>)
    (get-session self id)
    (let* ((lst (map url-split (query (get-data self id))))
           (info (get-info self id))
           (title (query-title info))
           (flag (if (string=? "0" (query-flag info)) #t #f))
           (za (open-output-zip-archive
                (fsencode (path-cleanup #`"mg,|id| ,|title|.zip")))))
      (for-each-with-index
       (^(i x) (receive (status header body)
                   (http-get (car x) ((if flag path-convert values) (cdr x)))
                 (let1 body (if (string=? "drm.seiga.nicovideo.jp" (car x))
                                (decrypt body (get-key-from-url (cdr x)))
                                body)
                   (zip-add-file za (format #f "~4,'0d.jpg" i)
                                 body :compression-level Z_NO_COMPRESSION))))
       lst)
      (zip-close za))))


(define (parse-nico-uri uri)
  (cond ((#/mg(\d+)(\?.+)?$/ uri) => (cut <> 1))
        (else (error "invalid id : " uri))))

(define file->list
  (cut call-with-input-file <> (pa$ port->list read-line)))

(define (usage cmd)
  (print "usage: " cmd " [options] [args]")
  (print "\
Options:
  --listfile file   reading list from file.
  --help            show usage.
")
  (exit))

(define (main args)
  (let-args (cdr args)
      ((listfile  "l|listfile=s"  #f)
       (help  "h|help" => (cut usage (car args)))
       . targets)
    (let ((targets (if listfile (file->list listfile) targets))
          (nico-obj (make <nico>)))
      (if (null? targets)
          (usage (car args))
          (begin
            (nico-login nico-obj *mail* *password*)
            (for-each ($ download-seiga nico-obj $ parse-nico-uri $)
                      targets))))))
