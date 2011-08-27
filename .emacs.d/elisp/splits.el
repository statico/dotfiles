;; http://www.emacswiki.org/emacs/ThreeWindows

;; People used to work with one, two or four windows per frame. They are
;; all good choices. Especially for four windows, it even looks funny.
;; People donate their screen space to 4 tasks in parallel, any time when
;; a task needs more space, just type C-x 1 to change to the full frame
;; and then type C-<left> to come back(WinnerMode). Its a really funny
;; way so that I had even made a function to split window in to 4 for me.

;; But quickly I noticed that I spent too many time in the endless C-x 1
;; and C-<left> cycles. So I start to work in 3 window mode – always keep
;; a relatively bigger window so that I don’t need to change so
;; frequently.

;; And since there are different size of windows, things must be able to
;; be changed from each. So I made these functions to fulfill these
;; requirements.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             window layout related               ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+

(defun split-window-4()
  "Splite window into 4 sub-window"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn (split-window-vertically)
             (split-window-horizontally)
             (other-window 2)
             (split-window-horizontally)
             )
    )
  )

;; split-v

;  +----------------------+                 +----------- +-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------------------+   +-------+/    |            |           |
;  |                      |           /     |            |           |
;  |                      |                 |            |           |
;  +----------------------+                 +----------- +-----------+

(defun split-v ()
  (interactive)
  (if (= 2 (length (window-list)))
      (let (( thisBuf (window-buffer))
            ( nextBuf (progn (other-window 1) (buffer-name))))
        (progn   (delete-other-windows)
                 (split-window-horizontally)
                 (set-window-buffer nil thisBuf)
                 (set-window-buffer (next-window) nextBuf)
                 ))
    )
  )

;; split-h

;  +----------- +-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            |           |    +-------+/    +----------------------+
;  |            |           |            /     |                      |
;  |            |           |                  |                      |
;  +----------- +-----------+                  +----------------------+

(defun split-h ()
  (interactive)
  (if (= 2 (length (window-list)))
      (let (( thisBuf (window-buffer))
            ( nextBuf (progn (other-window 1) (buffer-name))))
        (progn   (delete-other-windows)
                 (split-window-vertically)
                 (set-window-buffer nil thisBuf)
                 (set-window-buffer (next-window) nextBuf)
                 ))
    )
  )

;; split-v-3

;  +----------------------+                 +----------- +-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------------------+   +-------+/    |            |-----------|
;  |          |           |           /     |            |           |
;  |          |           |                 |            |           |
;  +----------------------+                 +----------- +-----------+


(defun split-v-3 ()
  "Change 3 window style from horizontal to vertical"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stBuf (window-buffer (car winList)))
              (2ndBuf (window-buffer (car (cdr winList))))
              (3rdBuf (window-buffer (car (cdr (cdr winList))))))
          (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
          (delete-other-windows)
          (split-window-horizontally)
          (set-window-buffer nil 1stBuf)
          (other-window 1)
          (set-window-buffer nil 2ndBuf)
          (split-window-vertically)
          (set-window-buffer (next-window) 3rdBuf)
          (select-window (get-largest-window))
          )
        )
    )
  )

;; split-h-3

;  +----------- +-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            |-----------|    +-------+/    +----------------------+
;  |            |           |            /     |           |          |
;  |            |           |                  |           |          |
;  +----------- +-----------+                  +----------------------+


(defun split-h-3 ()
  "Change 3 window style from vertical to horizontal"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stBuf (window-buffer (car winList)))
              (2ndBuf (window-buffer (car (cdr winList))))
              (3rdBuf (window-buffer (car (cdr (cdr winList))))))
          (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
          (delete-other-windows)
          (split-window-vertically)
          (set-window-buffer nil 1stBuf)
          (other-window 1)
          (set-window-buffer nil 2ndBuf)
          (split-window-horizontally)
          (set-window-buffer (next-window) 3rdBuf)
          (select-window (get-largest-window))
          )
        )
    )
  )

;; roll-v-3

;  +----------- +-----------+                    +----------- +-----------+
;  |            |     C     |            \       |            |     A     |
;  |            |           |    +-------+\      |            |           |
;  |     A      |-----------|    +-------+/      |     B      |-----------|
;  |            |     B     |            /       |            |     C     |
;  |            |           |                    |            |           |
;  +----------- +-----------+                    +----------- +-----------+
;
;  +------------------------+                     +------------------------+
;  |           A            |           \         |           B            |
;  |                        |   +-------+\        |                        |
;  +------------------------+   +-------+/        +------------------------+
;  |     B     |     C      |           /         |     C     |     A      |
;  |           |            |                     |           |            |
;  +------------------------+                     +------------------------+


(defun roll-v-3 ()
  "Rolling 3 window buffers clockwise"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stWin (car winList))
              (2ndWin (car (cdr winList)))
              (3rdWin (car (cdr (cdr winList)))))
          (let ((1stBuf (window-buffer 1stWin))
                (2ndBuf (window-buffer 2ndWin))
                (3rdBuf (window-buffer 3rdWin))
                )
            (set-window-buffer 1stWin 3rdBuf)
            (set-window-buffer 2ndWin 1stBuf)
            (set-window-buffer 3rdWin 2ndBuf)
            )
          )
        )
    )
  )

;; change-split-type

;; ‘split-v’ and ‘split-h’ may be merged into one function, that
;; automatically detects split type and changes vertical to horizontal
;; or vice-versa, like this:

;;  +----------------------+                +---------- +----------+
;;  |                      |          \     |           |          |
;;  |                      |  +-------+\    |           |          |
;;  +----------------------+  +-------+/    |           |          |
;;  |                      |          /     |           |          |
;;  |                      |                |           |          |
;;  +----------------------+                +---------- +----------+
;;
;;  +--------- +-----------+                +----------------------+
;;  |          |           |          \     |                      |
;;  |          |           |  +-------+\    |                      |
;;  |          |           |  +-------+/    +----------------------+
;;  |          |           |          /     |                      |
;;  |          |           |                |                      |
;;  +--------- +-----------+                +----------------------+

(defun change-split-type-2 ()
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive)
  (if (= 2 (length (window-list)))
      (let ((thisBuf (window-buffer))
            (nextBuf (progn (other-window 1) (buffer-name)))
            (split-type (if (= (window-width)
                               (frame-width))
                            'split-window-horizontally
                          'split-window-vertically)))
        (progn
          (delete-other-windows)
          (funcall split-type)
          (set-window-buffer nil thisBuf)
          (set-window-buffer (next-window) nextBuf)))))

;; change-split-type-3

;; ‘split-v-3’ and ‘split-h-3’ may also be merged into one function,
;; that automatically detects split type and changes vertical to
;; horizontal or vice-versa, like this:

;  +----------------------+                 +----------- +-----------+
;  |                      |           \     |            |           |
;  |                      |   +-------+\    |            |           |
;  +----------------------+   +-------+/    |            |-----------|
;  |          |           |           /     |            |           |
;  |          |           |                 |            |           |
;  +----------------------+                 +----------- +-----------+

;  +----------- +-----------+                  +----------------------+
;  |            |           |            \     |                      |
;  |            |           |    +-------+\    |                      |
;  |            |-----------|    +-------+/    +----------------------+
;  |            |           |            /     |           |          |
;  |            |           |                  |           |          |
;  +----------- +-----------+                  +----------------------+

(defun change-split-type-3 ()
  "Change 3 window style from horizontal to vertical and vice-versa"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
        (let ((1stBuf (window-buffer (car winList)))
              (2ndBuf (window-buffer (car (cdr winList))))
              (3rdBuf (window-buffer (car (cdr (cdr winList)))))
              (split-3
               (lambda(1stBuf 2ndBuf 3rdBuf split-1 split-2)
                 "change 3 window from horizontal to vertical and vice-versa"
                 (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
                 (delete-other-windows)
                 (funcall split-1)
                 (set-window-buffer nil 1stBuf)
                 (other-window 1)
                 (set-window-buffer nil 2ndBuf)
                 (funcall split-2)
                 (set-window-buffer (next-window) 3rdBuf)
                 (select-window (get-largest-window))
                 ))
              (split-type-1 nil)
              (split-type-2 nil)
              )
          (if (= (window-width) (frame-width))
              (setq split-type-1 'split-window-horizontally split-type-2 'split-window-vertically)
            (setq split-type-1 'split-window-vertically  split-type-2 'split-window-horizontally))
          (funcall split-3 1stBuf 2ndBuf 3rdBuf split-type-1 split-type-2)

))))


;; change-split-type

;; ‘change-split-type-2’ and ‘change-split-type-3’ can be further merged
;; into one function that automatically detects split type and changes
;; vertical to horizontal or vice-versa, like this:

(defun change-split-type ()
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive)
  (if (= 2 (length (window-list)))
      (change-split-type-2)
    (change-split-type-3)
    ))

;; A second check for there being 3 buffers can be added if more
;; complex functionality (such as for more than 3 windows) is added.
