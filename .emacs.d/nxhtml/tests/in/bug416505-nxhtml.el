;; NXHTML
;;(load (concat vendor-path "/nxhtml/autostart.el"))

(defconst mumamo-actionscript-tag-start-regex
  (rx "<mx:Script>" (0+ space) "<![CDATA["))

(defconst mumamo-actionscript-tag-end-regex
  (rx "]]>" (0+ space) "</mx:Script>"))

;; (defun mumamo-search-bw-exc-start-inlined-actionscript (pos min)
;;   (let ((exc-start (mumamo-chunk-start-bw-re pos min mumamo-actionscript-tag-start-regex)))
;;     (and exc-start
;;          (<= exc-start pos)
;;          (cons exc-start 'espresso-mode))))

;; (defun mumamo-search-bw-exc-end-inlined-actionscript (pos min)
;;   (mumamo-chunk-end-bw-re pos min mumamo-actionscript-tag-end-regex))

;; (defun mumamo-search-fw-exc-start-inlined-actionscript-old (pos max)
;;   (mumamo-chunk-start-fw-re pos max mumamo-actionscript-tag-start-regex))

(defun mumamo-search-fw-exc-start-inlined-actionscript (pos max)
  (let ((where (mumamo-chunk-start-fw-re pos max mumamo-actionscript-tag-start-regex)))
    (when where
      (list where 'js-mode))))

(defun mumamo-search-fw-exc-end-inlined-actionscript (pos max)
  (mumamo-chunk-end-fw-re pos max mumamo-actionscript-tag-end-regex))

(defun mumamo-chunk-inlined-actionscript (pos min max)
  ;; (mumamo-find-possible-chunk pos min max
  ;;                             'mumamo-search-bw-exc-start-inlined-actionscript
  ;;                             'mumamo-search-bw-exc-end-inlined-actionscript
  ;;                             'mumamo-search-fw-exc-start-inlined-actionscript-old
  ;;                             'mumamo-search-fw-exc-end-inlined-actionscript)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-actionscript
                                 'mumamo-search-fw-exc-end-inlined-actionscript))

(define-mumamo-multi-major-mode mxml-actionscript-mumamo-mode
    "Turn on multiple major modes for MXML with main mode `nxml-mode'.
This covers inlined style and script for mxml."
  ("nXml Family" nxml-mode (mumamo-chunk-inlined-actionscript)))
 
(add-to-list 'auto-mode-alist '("\\.mxml$" . mxml-actionscript-mumamo-mode))
