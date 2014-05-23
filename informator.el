
(setq p-informator-depth 1)

(defun p-informator-elisp-filter()
  "Rtns regex to filter elisp. depth 1 fn names; 2 also doc strings, 3 also ;;;"
  (cond 
   ((= p-informator-depth 1) "^\(defun")
   ((= p-informator-depth 2) "\\(^(defun\\)\\|\\(^  \"\\)")
   (t "\\(^(defun\\)\\|\\(^  \"\\)\\|\\(;;;\\)")))

(defun p-informator-filter()
  "returns default regexp for filtering current file type"
  (let ((bname (buffer-name)))
    (cond
     ((string-match ".el" bname)    (p-informator-elisp-filter))
     ((string-match ".emacs" bname) (p-informator-elisp-filter))
     ((string-match ".cpp" bname) "^///")
     ((string-match ".c" bname)   "^///")
     ((string-match ".h" bname)   "^///")
     (t nil))))

(defun p-informator-select()
  "handles selection in occur-mode: goes to selected code, maximizes its window"
  (interactive)
  (occur-mode-goto-occurrence)
  (delete-other-windows))

(define-key 
  occur-mode-map  
  (kbd "<RET>") 
  'p-informator-select)

(defun informator-occur(filter)
  "applies filter (has smart defaults) via occur-mode; maximizes results window"
  (interactive "sFilter: ")
  (let ( (filter (if (and filter (not (string= "" filter))) filter 
		   (p-informator-filter))))
    (occur filter)
    (other-window 1)
    (delete-other-windows)))

(defun informator-depth(iii)
  "Set int 1-3; controls how much info shown in occur-mode using default filter"
  (interactive "nDepth? ")
  (cond 
   ((>= iii 3) (setq p-informator-depth 3))
   ((<= iii 1) (setq p-informator-depth 1))
   (t (setq p-informator-depth 2))))
(window-body-width)

(defun p-open-right-pane()
  "private fn opens and weaks a pane on right"
  (when (not (get-buffer "*rp"))
    (let ( (wbd 126));;(window-body-width))
      (let ((left-width (cond
		       ((<= wbd 160) 80)
		       (t (- wbd 80)))))
	(delete-other-windows)
	(split-window-right left-width)
	(other-window 1))))
  (switch-to-buffer "*rp*")
  (visual-line-mode)
  (erase-buffer)
  (other-window 1))

(require 'thingatpt)
(defun informator-describe-function()
  "Calls describe-function for fn at point; shows info in pane to right"
  (interactive)
  (let ( (q (symbol-at-point))
	 (rp (get-buffer "*rp*")))
    (p-open-right-pane)
    (princ (describe-function q) rp)
    (visual-line-mode)))

(defun informator-apropos(str)
  "Shows results of apropos in pane to right"
  (interactive "sApropos: ")
  (let ( (rp (get-buffer "*rp*")))
    (p-open-right-pane)
    (princ (apropos str) rp)
    (visual-line-mode)))

(defun informator-ceeq(str)
  "Searches language specific documentation"
  (interactive "sCeeq: ")
  (let ((dir (p-informator-ceeq-dir)))
    (when dir
      (ceeq-search (concat str " dir: " dir)))))
	    
(defun p-informator-ceeq-dir()
  (let ( (bname (buffer-name)))
    (cond
     ((string-match ".el" bname)    "~/Documents/ORG/EMACS")
     ((string-match ".emacs" bname)    "~/Documents/ORG/EMACS")
     (t nil))))



;;; MORE
;; ceeq integration:
;;   dict in so can be added to from .emacs, holds per mode dir
;;   per project additions via config file
;;   also code search?
;; defns
;;   http://stackoverflow.com/questions/4222183/emacs-how-to-jump-to-function-definition-in-el-file   

;;; ALTERNATIVES
;; helm
;; imenu

;(p-open-right-pane)
;(get-buffer "*rp*")
;(get-buffer "*rap*")
