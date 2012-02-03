(use-modules (oop goops))

(define-class <item> (<entity>)
  (appearance #:init-value (make <map-element> #:r #\$ #:f '(0 255 0)) #:accessor appearance #:init-keyword #:appearance))

;; (define-class <wall> (<item>)
;;   (appearance #:init-value (make <map-element> #:r #\# #:f '(150 150 150)) #:accessor appearance #:init-keyword #:appearance))
