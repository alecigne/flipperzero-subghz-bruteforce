(defun prepend (lst &key (elt nil) (final-size 0))
  (let ((current-size (length lst)))
    (if (>= current-size final-size)
        lst
        (prepend (cons elt lst) :elt elt :final-size final-size))))

(defun int-to-bit-list (n)
  (labels ((helper (n acc)
             (multiple-value-bind (q r) (floor n 2)
               (if (zerop q) (cons r acc) (helper q (cons r acc))))))
    (helper n '())))

(defun bit-to-signal (bit)
  (cond ((= bit 0) (list -300 600))
        ((= bit 1) (list -600 300))
        (t nil)))

(defun bit-list-to-signal (bit-list)
  (let ((full-bit-list (prepend bit-list :elt 0 :final-size 12)))
    (append '(-12700 300) (mapcan #'bit-to-signal full-bit-list))))

(defun int-to-signal (n)
  (bit-list-to-signal (int-to-bit-list n)))

(defun signal-to-string (sig)
  (format nil "~{~A ~}" sig))

(defun generate-sub-file (filename &key ((:from start-key)) ((:to end-key)))
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (format f "Filetype: Flipper SubGhz RAW File
Version: 1
Frequency: 433920000
Preset: FuriHalSubGhzPresetOok650Async
Protocol: RAW~%")
    (loop for key from start-key to end-key do
      (format f "RAW_Data: ~a~%" (signal-to-string (int-to-signal key))))))
