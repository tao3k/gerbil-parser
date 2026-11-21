;;; -*- Gerbil -*-
;;; Pure borrowed-buffer traversal. Grammar supplies predicates, not cursors.
(export byte-span-skip byte-span-all?)

;; Callers own valid request bounds. Checked vector reads remain in place;
;; the result is an absolute offset, including until for an empty/all-hit span.
(definline (byte-span-skip (bytes : :u8vector) (from : :fixnum)
                     (until : :fixnum) accept?)
  => :fixnum
  (let loop ((cursor from))
    (if (and (fx< cursor until) (accept? (u8vector-ref bytes cursor)))
      (loop (fx+ cursor 1))
      cursor)))

(definline (byte-span-all? (bytes : :u8vector) (from : :fixnum)
                          (until : :fixnum) accept?)
  => :boolean
  (= (byte-span-skip bytes from until accept?) until))
