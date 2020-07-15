#|
 This file is a part of zippy
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.zippy)

;; compression.lisp
(docs:define-docs
  (function make-decompression-state
    "Create the state necessary to decompress given the requested compression format.

If required, the supplied buffer spec should be used to create the
output buffer for the decompression. The buffer spec may be either NIL
for a default buffer, an octet vector, or an integer for an octet
vector of the given size.

See CALL-WITH-DECOMPRESSED-BUFFER")
  
  (function call-with-decompressed-buffer
    "Call the supplied function with a decompressed buffer.

The function is called with three arguments:
  An octet buffer with the decompressed data
  A start index to the beginning of valid data
  An end index to the end of valid data

The supplied STATE must be a state obtained through
MAKE-DECOMPRESSION-STATE. The VECTOR, START, and END supply the octets
to decompress.

You may call this function repeatedly with new input to decompress
until the full region has been processed. The supplied function
likewise may be called multiple times per decompression run.

See MAKE-DECOMPRESSION-STATE")
  
  (function make-compression-state
    "Create the state necessary to compress given the requested compression format.

If required, the supplied buffer spec should be used to create the
output buffer for the compression. The buffer spec may be either NIL
for a default buffer, an octet vector, or an integer for an octet
vector of the given size.

See CALL-WITH-COMPRESSED-BUFFER")
  
  (function call-with-compressed-buffer
    "Call the supplied function with a compressed buffer.

The function is called with three arguments:
  An octet buffer with the compressed data
  A start index to the beginning of valid data
  An end index to the end of valid data

The supplied STATE must be a state obtained through
MAKE-COMPRESSION-STATE. The VECTOR, START, and END supply the octets
to compress.

You may call this function repeatedly with new input to compress
until the full region has been processed. The supplied function
likewise may be called multiple times per compression run.

See MAKE-COMPRESSION-STATE"))

;; decode.lisp
(docs:define-docs
  (type archive-file-required
    "Condition signalled if data from another disk (split file) is required.

When this condition is signalled, a USE-VALUE restart must be
available. If invoked, the value must be an IO value that supplies the
data of the requested DISK.

See DISK
See DECODE-FILE
See DECODE-ENTRY")
  
  (function with-zip-file
    "Open a zip file.

INPUT may be a pathname or string to designate a local file, a
seekable stream to read from, or an octet-vector to decode.

If decoding is successful, FILE will be bound to the resulting
ZIP-FILE instance. This instance is only valid within BODY. After
leaving the BODY, you may still access metadata and zip entries, but
you may not use DECODE-ENTRY to extract an entry's payload.

If the zip file is split and the supplied INPUT is a pathname
designator, other split disks will be determined automatically by
using the same pathname, but substituting the pathname-type by the
scheme z{DISK} . If INPUT is not a pathname designator, a condition of
type ARCHIVE-FILE-REQUIRED is signalled for every split disk that is
required.

See ZIP-FILE
See DECODE-FILE
See DECODE-ENTRY
See ARCHIVE-FILE-REQUIRED")

  (function decode-file
    "Decode the given IO into a ZIP-FILE.

If the zip file is split, will signal a condition of type
ARCHIVE-FILE-REQUIRED for every disk that is required to read the
central directory of the zip file.

May signal warnings if data mismatch or other correctable corruption
is detected in the zip file.

May signal an error if an incorrectable corruption is detected in the
zip file, or if the file is missing vital support structures that
would make it a valid zip file.

See WITH-ZIP-FILE
See ARCHIVE-FILE-REQUIRED")
  
  (function decode-entry
    "Decode the data payload of the ZIP-ENTRY

FUNCTION will be called repeatedly with three arguments:
  An octet buffer with the raw data
  A start index to the beginning of valid data
  An end index to the end of valid data

PASSWORD should be supplied if the entry is encrypted. If the entry is
encrypted, but no password is supplied, or the password is detectably
incorrect, an error is signalled. The password may be a string or an
octet-vector.

If an error is detected during decoding of the payload, an error is
signalled.

If the zip file is split a condition of type ARCHIVE-FILE-REQUIRED may
be signalled.

See ENTRY-TO-FILE
See ENTRY-TO-STREAM
See ENTRY-TO-VECTOR
See ARCHIVE-FILE-REQUIRED"))

;; encode.lisp
(docs:define-docs
  (function *version*
    "The max zip file version supported by this library.")
  
  (function *compatibility*
    "The default file attribute compatibility flag."))

;; encryption.lisp
(docs:define-docs
  (function make-decryption-state
    "Create the state necessary to decrypt given the requested encryption format.

If required, the supplied buffer spec should be used to create the
output buffer for the decryption. The buffer spec may be either NIL
for a default buffer, an octet vector, or an integer for an octet
vector of the given size.

The given password may be a string or octet vector supplying the
password for decryption.

See CALL-WITH-DECRYPTED-BUFFER")
  
  (function call-with-decrypted-buffer
    "Call the supplied function with a decrypted buffer.

The function is called with three arguments:
  An octet buffer with the decrypted data
  A start index to the beginning of valid data
  An end index to the end of valid data

The supplied STATE must be a state obtained through
MAKE-DECRPTIION-STATE. The VECTOR, START, and END supply the octets
to decrypt.

You may call this function repeatedly with new input to decrypt
until the full region has been processed. The supplied function
likewise may be called multiple times per decryption run.

See MAKE-DECRYPTION-STATE")
  
  (function make-encryption-state
    "Create the state necessary to encrypt given the requested encryption format.

If required, the supplied buffer spec should be used to create the
output buffer for the encryption. The buffer spec may be either NIL
for a default buffer, an octet vector, or an integer for an octet
vector of the given size.

See CALL-WITH-ENCRYPTED-BUFFER")
  
  (function call-with-encrypted-buffer
    "Call the supplied function with an encrypted buffer.

The function is called with three arguments:
  An octet buffer with the encrypted data
  A start index to the beginning of valid data
  An end index to the end of valid data

The supplied STATE must be a state obtained through
MAKE-ENCRYPTION-STATE. The VECTOR, START, and END supply the octets
to encrypt.

You may call this function repeatedly with new input to encrypt
until the full region has been processed. The supplied function
likewise may be called multiple times per encryption run.

See MAKE-ENCRYPTION-STATE"))

;; io.lisp
(docs:define-docs
  (type io
    "")
  
  (type vector-input
    "")
  
  (function vector-input-vector
    "")
  
  (function vector-input-index
    "")
  
  (function seek
    "")
  
  (function has-more
    "")
  
  (function index
    "")
  
  (function size
    "")
  
  (function ub32
    "")
  
  (function output
    "")
  
  (function parse-structure*
    "")
  
  (function write-structure*
    "")
  
  (function parse-structure
    "")
  
  (function with-io
    ""))

;; parser.lisp
(docs:define-docs
  (function decode-structure
    "")
  
  (function read-structure
    "")
  
  (function encode-structure
    "")
  
  (function write-structure
    "")
  
  (function define-byte-structure
    ""))

;; structures.lisp
(docs:define-docs
  (type zip64-extended-information
    "")
  
  (type os/2
    "")
  
  (type ntfs
    "")
  
  (type openvms
    "")
  
  (type unix
    "")
  
  (type patch-descriptor
    "")
  
  (type pkcs7-store
    "")
  
  (type x509-file
    "")
  
  (type x509-central-directory
    "")
  
  (type encryption-header
    "")
  
  (type record-management-controls
    "")
  
  (type pkcs7-encryption-recipient-certificate-list
    "")
  
  (type mvs
    "")
  
  (type policy-decryption-key-record
    "")
  
  (type key-provider-record
    "")
  
  (type policy-key-data-record
    "")
  
  (type zipit-macintosh-long
    "")
  
  (type zipit-macintosh-short-file
    "")
  
  (type zipit-macintosh-short-dir
    "")
  
  (type infozip-unicode-comment
    "")
  
  (type infozip-unicode-path
    "")
  
  (type data-stream-alignment
    "")
  
  (type microsoft-open-packaging-growth-hint
    "")
  
  (type aes-extra-data
    ""))

;; tables.lisp
(docs:define-docs
  (function file-attribute-name
    "")
  
  (function file-attribute-id
    "")
  
  (function compression-method-name
    "")
  
  (function compression-method-id
    "")
  
  (function encryption-method-name
    "")
  
  (function encryption-method-id
    ""))

;; zippy.lisp
(docs:define-docs
  (type zip-file
    "")
  
  (function entries
    "")
  
  (function disks
    "")
  
  (function comment
    "")
  
  (type zip-entry
    "")
  
  (function zip-file
    "")
  
  (function version
    "")
  
  (function attributes
    "")
  
  (function encryption-method
    "")
  
  (function compression-method
    "")
  
  (function crc-32
    "")
  
  (function disk
    "")
  
  (function start
    "")
  
  (function uncompressed-size
    "")
  
  (function extra-fields
    "")
  
  (function last-modified
    "")
  
  (function file-name
    "")
  
  (function comment
    "")
  
  (function content
    "")
  
  (function entry-to-file
    "")

  (function entry-to-stream
    "")

  (function entry-to-vector
    "")
  
  (function extract-zip
    "")
  
  (function compress-zip
    ""))
