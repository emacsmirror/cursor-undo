;;; cursor-undo.el --- Undo Cursor Movement -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author:       Luke Lee <luke.yx.lee@gmail.com>
;; Maintainer:   Luke Lee <luke.yx.lee@gmail.com>
;; Keywords:     undo, cursor
;; Version:      1.1.4

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Cursor-undo allows you to undo cursor movement commands using the Emacs
;; standard `undo' command.
;;
;; For frequent cursor movements such as up/down/left/right, it combines
;; the movements of the same direction into a single undo entry.  This
;; prevents the undo command from reversing each individual character
;; movement separately.  For example, if you move the cursor 20 characters
;; to the right and then 10 lines up, the first undo will go down 10 lines
;; back, and the next undo move back 20 characters left.  On the other
;; hand, for search commands that often jump across multiple pages, each
;; search command has its own undo entry, allowing you to undo them one at
;; a time rather than as a combined operation.
;;
;; This cursor-undo functionality has existed in my local Emacs init file
;; for over 11+ years, since version 0 on 2013-06-26.  It was originally
;; intended to support my Brief Editor Mode only, but I later found it
;; would be more useful if implemented in a more generalized way.  For
;; years I have hoped for an official implementation of this feature,
;; which is commonly seen among various editors.  Considering my
;; implementation using advice functions a bit inelegant so I have always
;; hesitated to release it till recently.
;;
;; Until there is official support for the cursor undo feature, this
;; package serves most common daily needs.  The core design is to align
;; with Emacs's native `undo' function by recording cursor positions
;; and screen-relative position undo entries in the `buffer-undo-list'
;; in accordance with its documentation.
;;
;; As this package primarily consists of advice functions to wrap cursor
;; movement commands, each cursor movement command needs to be manually
;; wrapped with `def-cursor-undo'.  For interactive functions that
;; heavily invoke advised cursor movement commands, you may even need to
;; advise them with `disable-cursor-tracking' to prevent generating
;; numerous distinct cursor undo entries from a single command.  For user
;; convenience, we have prepared ready `def-cursor-undo' advice sets for
;; standard Emacs cursor movement commands, Brief Editor mode, Viper
;; mode, and EVIL mode.
;;
;; Usage:
;;
;;   Once this package is installed, you only need to enable cursor-undo
;;   mode by adding the following line into your Emacs init file .emacs or
;;   init.el:
;;
;;     (cursor-undo 1)
;;
;; Notes for EVIL mode user:
;;
;;   If you choose to use default Emacs `undo' system, you should be able
;;   to use `evil-undo' to undo cursor movements.  If your choice is
;;   tree-undo or another undo system, you might need to use Emacs default
;;   `undo' (C-_, C-/ or C-x u ...) to undo cursor movements.
;;
;; Notes for Viper mode user:
;;
;;   The default `viper-undo' is advised to allow cursor-undo.  If you
;;   find the advised function not working properly, consider comment out
;;   the following source code `(define-advice viper-undo ...' to restore
;;   the original `viper-undo' function and use Emacs default `undo'
;;   (C-_, C-/ or C-x u ...) to undo cursor movements.
;;
;; Future TODO thoughts: a more desired implementation is to integrate it
;; into Emacs source by extending the `interactive' spec to add code
;; letters for various cursor undo information.
;;
;; Luke Lee [2024-07-19 Fri.]

;;; Code:

(defgroup cursor-undo nil
  "Cursor movement undo support."
  :prefix "cundo-"
  :group 'cursor-undo)

;; Global enabling control flag
;;;###autoload
(defcustom cundo-enable-cursor-tracking  nil
  "Global control flag to enable cursor undo tracking."
  :require 'cursor-undo
  :type 'boolean)

;;;###autoload
(define-minor-mode cursor-undo
  "Global minor mode for tracking cursor undo."
  :lighter " cu"
  :variable cundo-enable-cursor-tracking)

;; Local disable flag, use reverse logic as NIL is still a list and we can
;; pop it again and again
(defvar-local cundo-disable-local-cursor-tracking nil)

;; Clear duplicated `(apply cdr nil) nil' pairs in `buffer-undo-list' done by
;; `primitive-undo'.
(defcustom cundo-clear-usless-undoinfo t
  "Clean up obsolete undo info in the `buffer-undo-list'."
  :type 'boolean)

(defun cundo-restore-win (start)
  (set-window-start nil start)
  (if cundo-clear-usless-undoinfo
      ;; This tries to remove as much garbage as possible but still some left
      ;; in the `buffer-undo-list'.  TODO: add a idle task to remove them
      (while (and (null (car buffer-undo-list)) ;; nil
                  (equal (cadr buffer-undo-list) '(apply cdr nil))
                  (null (caddr buffer-undo-list)))
        ;; (nil (apply cdr nil) nil a b...) -> (nil a b...)
        (setq buffer-undo-list (cddr buffer-undo-list)))))

;; Note that this `prev-screen-start' is NOT a dynamic binding variable.
;; It's defined here to make byte compiler not to complain about:
;; "Warning: Unused lexical variable ‘prev-screen-start’".
(defvar prev-screen-start)

(defun cundo-track-screen-start (prv-screen-start)
  (let ((entry (list 'apply 'cundo-restore-win prv-screen-start)))
    (if (eq buffer-undo-list 't)
        (setq buffer-undo-list entry)
      (push entry buffer-undo-list))))

(defun cundo-track-prev-point (prev-point)
  (if (eq buffer-undo-list 't)
      (setq buffer-undo-list (list prev-point))
    (push prev-point buffer-undo-list)))

;;;###autoload
(defmacro def-cursor-undo (func-sym &optional no-combine screen-pos no-move)
  "Define an advice for FUNC-SYM to track cursor movements in the undo buffer.
The NO-COMBINE flag track each movement without combining the same
commands into a single undo record.  For example, multiple arrow key
movements like `right-chars' will be combined as a single undo operation
when NO-COMBINE is NIL.  Nested/reentering cursor-undo are prevented
using `cursor-tracking' and `cundo-enable-cursor-tracking' dynamic
variables.

Parameters:
  NO-COMBINE: (default NIL)
         Force adding an undo entry in undo buffer without combined with
         the previous command, if the previous command (`last-command')
         is the same as `this-command'.

  SCREEN-POS: (default NIL)
         Record window cursor relative position in undo buffer entry so
         that we can jump back undo editing and still having the cursor
         at the original relative position to the window.

  NO-MOVE: (default NIL)
         Add an undo entry even if cursor (`point') not moved.  For
         example, `recenter' won't move `point' but only the relative
         position of the cursor to the current window."

  (if (and (not screen-pos)
           no-move)
      (error
"Error: No undo information will be stored if we're neither recording cursor \
relative screen position (screen-pos=NIL) nor `point' position (no-move=t)."))
  (let* ((func-sym-str (symbol-name func-sym))
         (advice-sym-str (concat func-sym-str "-cursor-undo"))
         (advice-sym (make-symbol advice-sym-str))
         (def-advice-sym (intern-soft
                          (concat func-sym-str "@" advice-sym-str))))
    ;; prevent duplicate definition
    (when def-advice-sym
      (unless (member #'package-menu--post-refresh post-command-hook)
        ;; do not warn when upgrading this package
        (warn "Redefining cursor undo advice for `%S'" func-sym))
      (advice-remove func-sym def-advice-sym))
    `(define-advice ,func-sym (:around (orig-func &rest args) ,advice-sym)
       (let* ((cursor-tracking cundo-enable-cursor-tracking)
              ;; prevent nested calls for complicated compound commands
              (cundo-enable-cursor-tracking nil)
              (prev-point (point))
              (prev-screen-start)
              (result))
         ,@(when screen-pos
             '((if cursor-tracking
                   (setq prev-screen-start (window-start)))))
         (setq result (apply orig-func args))
         ;; This is a helper for commands that might take long. eg. page-up/
         ;; page-down in big files, or line-up/down in big files when marking.
         (unless
             (or (not cursor-tracking)
                 ;;[2017-11-15 Wed] Still need to test
                 ;;  `(called-interactively-p 'any)', why?  Maybe it's because
                 ;;  too many functions are invoked non-interactively and thus
                 ;;  produce a lot of undo records in the undo
                 ;;  buffer. Therefore after a search operation there are tons
                 ;;  and tons of cursor undo information to redo.  Therefore,
                 ;;  testing `(called-interactively-p 'any)' will be safer.
                 ;;
                 ;;[2017-11-13 Mon] We've already prevent reentering so there
                 ;;  is really no need to test if this call is called
                 ;;  interactively or not.  When a keyboard command calls
                 ;;  another keyboard command using normal LISP function calls
                 ;;  the (called-interactively-p 'any) will return nil unless
                 ;;  they are called using `call-interactively'.  Now we
                 ;;  remove it to allow either case.
                 ;;
                 ;; A sample function is this:
                 ;;  (def-cursor-undo line-bookmark-jump-nearest   t  t)
                 ;;  ;;(def-cursor-undo line-bookmark-nearest-next t  t)
                 ;;  ;;(def-cursor-undo line-bookmark-nearest-prev t  t)
                 ;;
                 ;; `line-bookmark-nearest-next'/`line-bookmark-nearest-prev'
                 ;; calls `line-bookmark-jump-nearest' (non-interactive call).
                 ;; By adding cursor-undo to the inner function
                 ;; `line-bookmark-jump-nearest' we don't need to add to both
                 ;; `line-bookmark-nearest-next'/`line-bookmark-nearest-prev'.
                 (not (called-interactively-p 'any))
                 (car cundo-disable-local-cursor-tracking)
                 ,@(unless no-combine '((eq last-command this-command)))
                 ;; if NO-MOVE is specified, check if `point' moved
                 ,@(unless no-move '((= prev-point (point))))
                 ;; Sometimes the buffer-undo-list is t
                 (and (listp buffer-undo-list)
                      (numberp (cadr buffer-undo-list))
                      (= prev-point (cadr buffer-undo-list))))
           ,@(if screen-pos
                 '((cundo-track-screen-start prev-screen-start)))
           ,@(unless no-move
               '((cundo-track-prev-point prev-point)))
           ;;(abbrevmsg (format "c=%S,%S b=%S" last-command this-command
           ;;                   buffer-undo-list) 128) ;; DBG
           (undo-boundary))
         result))))

;;
;; Disable cursor tracking during miscellaneous operations that could cause
;; temporarily cursor jump
;;
(defmacro disable-cursor-tracking (func-sym)
  (let* ((func-sym-str (symbol-name func-sym))
         (advice-sym-str (concat func-sym-str "-disable-cursor-tracking"))
         (advice-sym (make-symbol advice-sym-str))
         (def-advice-sym (intern-soft
                          (concat func-sym-str "@" advice-sym-str))))
    ;; prevent duplicate definition
    (when def-advice-sym
      (unless (member #'package-menu--post-refresh post-command-hook)
        ;; do not warn when upgrading this package
        (warn "Redefining cursor tracking disabling advice for `%S'" func-sym))
      (advice-remove func-sym def-advice-sym))
    `(define-advice ,func-sym (:around (orig-func &rest args) ,advice-sym)
       (let ((cundo-enable-cursor-tracking nil))
         (apply orig-func args)))))

;;
;; Allow cursor undo in a read-only buffer
;;   Notice that the original definition of undo is (interactive "*P") which
;;   means it cannot be performed in a read-only buffer. Here we allow the undo
;;   operation as long as the pending undo list is still a cursor movement.
;;
;;  This also enables a keyboard trick:
;;   If you just edited a big file and moved the cursor to browse the
;;   other parts, but forgot where you were, you can undo cursor
;;   movements to go back to your last edited position by long-holding
;;   <undo> till the last editing command.  However, you risk missing
;;   your last edited operation as it might just flash by so quickly that
;;   you don't even notice and keep undoing other cursor commands you
;;   don't want to undo at all.  In this case, you can switch the buffer
;;   to read-only mode (by setting `buffer-read-only' to 't), then long
;;   press <undo> untill the undo command warns that you that you're
;;   trying to edit a read-only buffer.  At this point you're exactly at
;;   the latest editing position where you are looking for.  Now you can
;;   then safely set `buffer-read-only' back to NIL and continue your
;;   editing.
;;
(define-advice undo (:around (orig-func &rest args)
                             undo-cursor-in-read-only-buffer)
  (interactive "P") ;; Change the behavior from "*P" to "P"
  (if (if (eq last-command 'undo)
          ;; last-command is undo, check pending undo list if the first command
          ;; is cursor movement or not
          (and (listp pending-undo-list)
               (numberp (car pending-undo-list)))
        ;; not a continuous undo, check first command is cursor movement or not
        (and (listp buffer-undo-list)
             (null (car buffer-undo-list))
             (numberp (cadr buffer-undo-list))))
      (apply orig-func args)
    (if buffer-read-only
        (if (listp pending-undo-list)
            (user-error "Buffer is read-only: cannot undo an editing command!")
          (apply orig-func args))
      (apply orig-func args))))

;;;
;;; Advice cursor movement commands
;;;

;; -----------------------------------------------------------------------
;;               keyboard function       no-combine  screen-pos  no-move
;; -----------------------------------------------------------------------
;; Emacs general cursor movements
(def-cursor-undo previous-line                  nil     nil      nil)
(def-cursor-undo next-line                      nil     nil      nil)
(def-cursor-undo left-char                      nil     nil      nil)
(def-cursor-undo right-char                     nil     nil      nil)
(def-cursor-undo scroll-up-command              nil     t)
(def-cursor-undo scroll-down-command            nil     t)
(def-cursor-undo scroll-left                    t       t)
(def-cursor-undo scroll-right                   t       t)
(def-cursor-undo beginning-of-buffer            t       t)
(def-cursor-undo end-of-buffer                  t       t)
(def-cursor-undo backward-word                  nil     nil)
(def-cursor-undo forward-word                   nil     nil)
(def-cursor-undo move-beginning-of-line)
(def-cursor-undo move-end-of-line)
(def-cursor-undo forward-sentence)
(def-cursor-undo backward-sentence)
(def-cursor-undo forward-paragraph              nil     t)
(def-cursor-undo backward-paragraph             nil     t)

;; Mouse movement, scrolling
(def-cursor-undo mouse-set-point                t)
(def-cursor-undo scroll-bar-toolkit-scroll      nil)
(def-cursor-undo mwheel-scroll                  nil)

;; Enabling `forward-sexp' will cause semantic parsing to push a lot of cursor
;; undo entries into the buffer undo list.
(def-cursor-undo forward-sexp                   t)
(def-cursor-undo backward-sexp                  t)
(def-cursor-undo mouse-drag-region              t)

;; Search
(def-cursor-undo isearch-forward                nil     t)
(def-cursor-undo isearch-backward               nil     t)
(def-cursor-undo isearch-forward-regexp         nil     t)
(def-cursor-undo isearch-backward-regexp        nil     t)

;; Others
(def-cursor-undo recenter                       nil     t       t)
(def-cursor-undo recenter-top-bottom            nil     t       t)
(def-cursor-undo mark-whole-buffer              t       t)
(def-cursor-undo goto-line                      t       t)
(def-cursor-undo move-to-window-line            t)
(def-cursor-undo jump-to-register               t       t)

(disable-cursor-tracking save-buffer)
(disable-cursor-tracking write-file)
;; tabify
(disable-cursor-tracking tabify)
(disable-cursor-tracking untabify)

;;
;; CUA rectangle, also used by Brief Editor mode
;;
(def-cursor-undo cua-resize-rectangle-up        nil     nil      nil)
(def-cursor-undo cua-resize-rectangle-down      nil     nil      nil)
(def-cursor-undo cua-resize-rectangle-left      nil     nil      nil)
(def-cursor-undo cua-resize-rectangle-right     nil     nil      nil)
(def-cursor-undo cua-resize-rectangle-page-up   nil     nil      nil)
(def-cursor-undo cua-resize-rectangle-page-down nil     nil      nil)

;;
;; Brief Editor Mode cursor movements
;;
;; For feature 'brief
;; If we defined brief-left-char and brief-right-char, remember to check
;; (brief-rectangle-active) and call cua-resize-rectangle-left/right
;; accordingly
;; -----------------------------------------------------------------------
;;               keyboard function       no-combine  screen-pos  no-move
;; -----------------------------------------------------------------------
(def-cursor-undo brief-previous-line            nil     nil    nil)
(def-cursor-undo brief-next-line                nil     nil    nil)
(def-cursor-undo brief-fixed-cursor-page-up     nil     t     nil)
(def-cursor-undo brief-fixed-cursor-page-down   nil     t     nil)
(def-cursor-undo brief-home                     t       t)
(def-cursor-undo brief-end                      t       t)
(def-cursor-undo brief-forward-word             nil)
(def-cursor-undo brief-backward-word            nil)
(def-cursor-undo brief-recenter-left-right      nil     t     t)
(def-cursor-undo brief-move-to-window-line-0    nil     t)
(def-cursor-undo brief-move-to-window-line-end  nil     t)
(def-cursor-undo brief-previous-physical-line   nil     nil)
(def-cursor-undo brief-next-physical-line       nil     nil)
(def-cursor-undo brief-beginning-of-file        nil     t)
(def-cursor-undo brief-end-of-file              nil     t)
(def-cursor-undo brief-mark-move-to-window-line-0 nil   t)
(def-cursor-undo brief-mark-move-to-window-line-end nil t)
;; Search
(def-cursor-undo brief-search-forward           t       t)
(def-cursor-undo brief-search-forward-currword  t       t)
(def-cursor-undo brief-search-backward          t       t)
(def-cursor-undo brief-search-backward-currword t       t)
(def-cursor-undo brief-repeat-search            t       t)
(def-cursor-undo brief-repeat-search-backward   t       t)
;; Bookmark related
(def-cursor-undo brief-bookmark-do-jump         t       t)
(def-cursor-undo brief-bookmark-jump-set-0      t       t)
(def-cursor-undo brief-bookmark-jump-set-1      t       t)
(def-cursor-undo brief-bookmark-jump-set-2      t       t)
(def-cursor-undo brief-bookmark-jump-set-3      t       t)
(def-cursor-undo brief-bookmark-jump-set-4      t       t)
(def-cursor-undo brief-bookmark-jump-set-5      t       t)
(def-cursor-undo brief-bookmark-jump-set-6      t       t)
(def-cursor-undo brief-bookmark-jump-set-7      t       t)
(def-cursor-undo brief-bookmark-jump-set-8      t       t)
(def-cursor-undo brief-bookmark-jump-set-9      t       t)
(def-cursor-undo brief-shift-tab                t       t)

(define-advice brief-buffer-read-only-toggle
    (:around (orig-func &rest args)
             read-only-toggle-not-go-into-last-command)
  ;; Let it not go into `last-command' so that it won't break our
  ;; continuous undo operations.  If our desire operation (using the above
  ;; read-only+undo trick) is
  ;;   "<toggle R/O> - <undo>s till stop - <toggle R/O> - keep undoing",
  ;; it will become
  ;;   "<toggle R/O> - <undo>s till stop - <toggle R/O> - REDOs !!!".
  ;; We don't want it start redoing! That's mainly because <undo> detected
  ;; the previous command is not an <undo> but a <toggle read-only>. By
  ;; setting last-command back (technically, set `this-command' back since
  ;; the caller will put this-command to last-command), we allow undos to
  ;; keep going.
  (let ((lastcmd last-command))
    (apply orig-func args)
    (setf this-command lastcmd)))

;; Bookmark related
;; For feature 'bookmark+-1 (emacswiki bookmark extension)
(def-cursor-undo bookmark-jump                  t      t) ;; C-x b g
(def-cursor-undo bookmark-jump-other-window     t      t) ;; C-x b j
(disable-cursor-tracking bookmark-save)

;;
;; Prevent cursor tracking during semantic parsing
;;
(eval-after-load 'semantic
  '(progn
     (add-hook 'semantic-before-idle-scheduler-reparse-hooks
               #'(lambda ()
                   (push 't cundo-disable-local-cursor-tracking)))
     (add-hook 'semantic-after-idle-scheduler-reparse-hooks
               #'(lambda ()
                   (pop cundo-disable-local-cursor-tracking)))))
(disable-cursor-tracking semantic-fetch-tags)
(disable-cursor-tracking senator-parse)
(disable-cursor-tracking senator-force-refresh)
(disable-cursor-tracking semantic-go-to-tag)

;; For feature 'smie
;; Need to disable the following, a sample test without disabling this is
;; to open a shell-script and place cursor on 'if' or 'else' or 'fi', and
;; try to move the cursor up/down. It will stuck at 'if','else', or 'fi'.
(disable-cursor-tracking smie-backward-sexp)
(disable-cursor-tracking smie-forward-sexp)
(disable-cursor-tracking smie-backward-sexp-command)
(disable-cursor-tracking smie-forward-sexp-command)

;;
;; Disable cursor tracking during ediff comparing [2013-06-28 15:16:06 +0800]
;;
(defvar undo-cursor-ediff-buffer-list nil)
(defun undo-cursor-ediff-prepare-buffer-hook ()
  (push (current-buffer) undo-cursor-ediff-buffer-list)
  (push 't cundo-disable-local-cursor-tracking)
  (message "Disable buffer %S cursor tracking" (current-buffer)))

(defun undo-cursor-ediff-cleanup-hook ()
  (dolist (ediff-buf undo-cursor-ediff-buffer-list)
    (with-current-buffer ediff-buf
      (pop cundo-disable-local-cursor-tracking)
      (message "Enable buffer %S cursor tracking" ediff-buf)))
  (setf undo-cursor-ediff-buffer-list nil))

(eval-after-load 'ediff
  '(progn
     (add-hook 'ediff-prepare-buffer-hook
               #'undo-cursor-ediff-prepare-buffer-hook)
     ;; Set-up two cleanup hooks in case of any error
     (add-hook 'ediff-cleanup-hook #'undo-cursor-ediff-cleanup-hook)
     (add-hook 'ediff-quit-hook #'undo-cursor-ediff-cleanup-hook)))

;; [2015-11-11 17:05:07 +0800] We don't need to track cursor when debugging.
;; This is for GUD functions which will call `recenter' from time to time (not
;; just my `gud-wrapper:display-line-recenter' function that calls `recenter',
;; some GUD functions seems to do so too).
;; For feature 'gud
(disable-cursor-tracking gud-filter)

;;
;; EVIL mode cursor movements support
;;
;; for feature 'evil-commands
(def-cursor-undo evil-previous-line                     nil     nil    nil)
(def-cursor-undo evil-next-line                         nil     nil    nil)
(def-cursor-undo evil-previous-visual-line              nil     nil    nil)
(def-cursor-undo evil-next-visual-line                  nil     nil    nil)
(def-cursor-undo evil-forward-char                      nil     nil    nil)
(def-cursor-undo evil-backward-char                     nil     nil    nil)
(def-cursor-undo evil-beginning-of-line                 t       t)
(def-cursor-undo evil-end-of-line                       t       t)
(def-cursor-undo evil-beginning-of-visual-line          t       t)
(def-cursor-undo evil-end-of-visual-line                t       t)
(def-cursor-undo evil-line                              t       t)
(def-cursor-undo evil-line-or-visual-line               t       t)
(def-cursor-undo evil-end-of-line-or-visual-line        t       t)
(def-cursor-undo evil-middle-of-visual-line             t       t)
(def-cursor-undo evil-percentage-of-line                t       t)
(def-cursor-undo evil-first-non-blank                   t       t)
(def-cursor-undo evil-last-non-blank                    t       t)
(def-cursor-undo evil-first-non-blank-of-visual-line    t       t)
(def-cursor-undo evil-next-line-first-non-blank         t       t)
(def-cursor-undo evil-next-line-1-first-non-blank       t       t)
(def-cursor-undo evil-previous-line-first-non-blank     t       t)
(def-cursor-undo evil-goto-line                         t       t)
(def-cursor-undo evil-goto-first-line                   t       t)
(def-cursor-undo evil-forward-word-begin                nil     nil)
(def-cursor-undo evil-forward-word-end                  nil     nil)
(def-cursor-undo evil-backward-word-begin               nil     nil)
(def-cursor-undo evil-backward-word-end                 nil     nil)
(def-cursor-undo evil-forward-WORD-begin                nil     nil)
(def-cursor-undo evil-forward-WORD-end                  nil     nil)
(def-cursor-undo evil-backward-WORD-begin               nil     nil)
(def-cursor-undo evil-backward-WORD-end                 nil     nil)
(def-cursor-undo evil-forward-section-begin             nil     t)
(def-cursor-undo evil-forward-section-end               nil     t)
(def-cursor-undo evil-backward-section-begin            nil     t)
(def-cursor-undo evil-backward-section-end              nil     t)
(def-cursor-undo evil-forward-sentence-begin            nil     nil)
(def-cursor-undo evil-backward-sentence-begin           nil     nil)
(def-cursor-undo evil-forward-paragraph                 nil     t)
(def-cursor-undo evil-backward-paragraph                nil     t)
(def-cursor-undo evil-jump-item                         t       t)
(def-cursor-undo evil-next-flyspell-error               t       t)
(def-cursor-undo evil-prev-flyspell-error               t       t)
(def-cursor-undo evil-previous-open-paren               t       t)
(def-cursor-undo evil-next-close-paren                  t       t)
(def-cursor-undo evil-previous-open-brace               t       t)
(def-cursor-undo evil-next-close-brace                  t       t)
(def-cursor-undo evil-next-mark                         t       t)
(def-cursor-undo evil-next-mark-line                    t       t)
(def-cursor-undo evil-previous-mark                     t       t)
(def-cursor-undo evil-previous-mark-line                t       t)
(def-cursor-undo evil-find-char                         t       t)
(def-cursor-undo evil-find-char-backward                t       t)
(def-cursor-undo evil-find-char-to                      t       t)
(def-cursor-undo evil-find-char-to-backward             t       t)
(def-cursor-undo evil-repeat-find-char                  t       t)
(def-cursor-undo evil-repeat-find-char-reverse          t       t)
(def-cursor-undo evil-goto-column                       t       t)
(def-cursor-undo evil-jump-backward                     t       t)
(def-cursor-undo evil-jump-forward                      t       t)
(def-cursor-undo evil-jump-backward-swap                t       t)
(def-cursor-undo evil-jump-to-tag                       t       t)
(def-cursor-undo evil-lookup                            t       t)
(def-cursor-undo evil-ret                               t       t)
(def-cursor-undo evil-ret-and-indent                    t       t)
(def-cursor-undo evil-window-top                        nil     t)
(def-cursor-undo evil-window-middle                     nil     t)
(def-cursor-undo evil-window-bottom                     nil     t)
(def-cursor-undo evil-visual-restore                    t       t)
(def-cursor-undo evil-visual-exchange-corners           t       t)
(def-cursor-undo evil-search-forward                    t       t)
(def-cursor-undo evil-search-backward                   t       t)
(def-cursor-undo evil-search-next                       t       t)
(def-cursor-undo evil-search-previous                   t       t)
(def-cursor-undo evil-search-word-backward              t       t)
(def-cursor-undo evil-search-word-forward               t       t)
(def-cursor-undo evil-search-unbounded-word-backward    t       t)
(def-cursor-undo evil-search-unbounded-word-forward     t       t)
(def-cursor-undo evil-goto-definition                   t       t)
(def-cursor-undo evil-ex-search-next                    t       t)
(def-cursor-undo evil-ex-search-previous                t       t)
(def-cursor-undo evil-ex-search-forward                 t       t)
(def-cursor-undo evil-ex-search-backward                t       t)
(def-cursor-undo evil-ex-search-word-forward            t       t)
(def-cursor-undo evil-ex-search-word-backward           t       t)
(def-cursor-undo evil-ex-search-unbounded-word-forward  t       t)
(def-cursor-undo evil-ex-search-unbounded-word-backward t       t)

;;
;; Viper mode cursor movements support
;;
;; for feature 'viper-cmd
(define-advice viper-undo (:around (orig-func &rest args) cundo-viper-undo)
  (let ((bu1 (car buffer-undo-list))
        (bu2 (cadr buffer-undo-list)))
    (if (not (or (and (null bu1)
                      (or (integerp bu2)
                          (and (eq (car bu2) #'apply)
                               (eq (cadr bu2) #'cundo-restore-win))))
                 (and (integerp bu1) (null bu2))
                 (and (eq (car bu1) #'apply)
                      (eq (cadr bu1) #'cundo-restore-win))))
        (apply orig-func args)
      ;; We're at a cursor-undo entry, use Emacs native undo
      (setq this-command 'undo)
      (undo))))
(def-cursor-undo viper-backward-Word)
(def-cursor-undo viper-end-of-Word)
(def-cursor-undo viper-find-char-backward)
(def-cursor-undo viper-goto-line                        t       t)
(def-cursor-undo viper-window-top                       t       t)
(def-cursor-undo viper-window-bottom                    t       t)
(def-cursor-undo viper-window-middle                    t       t)
(def-cursor-undo viper-search-Next                      t       t)
(def-cursor-undo viper-goto-char-backward)
(def-cursor-undo viper-forward-Word)
(def-cursor-undo viper-brac-function)
(def-cursor-undo viper-ket-function)
(def-cursor-undo viper-bol-and-skip-white)
(def-cursor-undo viper-goto-mark                        t       t)
(def-cursor-undo viper-backward-word)
(def-cursor-undo viper-end-of-word)
(def-cursor-undo viper-find-char-forward)
(def-cursor-undo viper-backward-char)
(def-cursor-undo viper-previous-line)
(def-cursor-undo viper-forward-char)
(def-cursor-undo viper-search-next                      t       t)
(def-cursor-undo viper-goto-char-forward)
(def-cursor-undo viper-forward-word)
(def-cursor-undo viper-line-to-top                      t       t       t)
(def-cursor-undo viper-line-to-middle                   t       t       t)
(def-cursor-undo viper-line-to-bottom                   t       t       t)
(def-cursor-undo viper-backward-paragraph               nil     t)
(def-cursor-undo viper-goto-col)
(def-cursor-undo viper-forward-paragraph                nil     t)
(def-cursor-undo viper-goto-eol)
(def-cursor-undo viper-paren-match                      t       t)
(def-cursor-undo viper-goto-mark-and-skip-white         t       t)
(def-cursor-undo viper-backward-sentence                t       t)
(def-cursor-undo viper-forward-sentence                 t       t)
(def-cursor-undo viper-next-line-at-bol)
(def-cursor-undo viper-repeat-find-opposite             t       t)
(def-cursor-undo viper-previous-line-at-bol)
(def-cursor-undo viper-search-forward                   t       t)
(def-cursor-undo viper-beginning-of-line)
(def-cursor-undo viper-repeat-find                      t       t)

(provide 'cursor-undo)

;;; cursor-undo.el ends here
