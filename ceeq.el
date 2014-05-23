
(setq p-ceeq-filter "")

(defun p-ceeq-extract-path(line)
  "Takes line from recoll results, returns org mode link in [[]] or nil if none"
  (let ( (fields (split-string line "\t")))
    (when (< 2 (length fields))
      (concat "[" (nth 1 fields) "]\n\n"))))

(defun p-ceeq-out(term results)
  "Takes search term & recoll output; prints to target file"
  (let ( (buf (get-buffer p-ceeq-target)))
    (princ (concat "* <search=" term ">\n") buf)
    (dolist (line (split-string results "\n"))
      (let ( (path (p-ceeq-extract-path line)))
	(when path (princ path buf))))
    (princ "</search>\n" buf)))

(defun p-ceeq-search(term)
  "Does recoll search"
  (when term
    (find-file p-ceeq-target)
    (switch-to-buffer p-ceeq-target)
    (ceeq-delete-search term)
    (end-of-buffer);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hack
    (p-ceeq-out term
		(shell-command-to-string 
		 (concat "recoll -t " term))))) 

(defun p-ceeq-extract-search (line)
  "Returns search term stored in line if is one; else nil"
  (if (string-match "\\<search=\\(.*\\)\\>" line)
      (match-string 1 line)
    nil))

(defun p-ceeq-input()
  "Rtns whatever searchable thing is under cursor - stored search, word, or nil"
  (interactive)
  (let ( (line (p-ceeq-extract-search (thing-at-point 'line)))
	 (word (thing-at-point 'word))) 
    (cond (line line p-ceeq-filter)
	  (word (concat word p-ceeq-filter))
	  (t nil))))

(defun ceeq-search (term)
  "Does search. Takes search term as string or searches on whatever is under cursor"
  (interactive "sSearch: ")
  (p-ceeq-search (if (and term (not (string= "" term))) (concat term p-ceeq-filter)
		   (p-ceeq-input))))
  
(defun ceeq-set-target (&optional filename)
  "Sets target file for search output. Must end in org"
  (interactive)
;;; TO DO add .org if missing
  (setq p-ceeq-target (if filename filename
			"temp.org")))

(defun ceeq-set-filter(&optional str)
  "Set an extra term that is added to searches."
  (interactive)
  (setq p-ceeq-filter (if str (concat " AND (" str ")")
			"")))

(ceeq-set-target)
(ceeq-set-filter)

(global-set-key "\C-q" 'ceeq-search)

(defun ceeq-delete-search (term) ;; assumes in correct buffer
  "Deletes search; must be in same buffer"
  (interactive "sDelete: ")
  (save-excursion 
    (beginning-of-buffer)
    (when (> (buffer-size) 0)
      (when (re-search-forward (concat "<search=" term ">") (buffer-size) t)
	(beginning-of-visual-line)
	(let ( (a (point)))
	  (when (search-forward "</search>" (buffer-size) t)
	    (delete-region a (point))))))))

;; NEXT
;; filter term builder
;; add date to search
;; update whole org file

;;(ceeq-set-filter "crab sticks")
;;(ceeq-set-filter)
;;(ceeq-search "goldfish")
;;(ceeq-search "trout -jedi")
;;(ceeq-search "ratsat")


;; cqq - get rid of append; always erases matching search
;;     - does update if used on existing search (does NOT apply current filter)
;; cqo - output
;; cqf - if no text in mbfr, will load filter from bfr if one is 
;;       under point, otherwise activates a filter building menu
;; cqw

;; C-r =? turn back to emacs reverse isearch


;; <search=>
;; <r>[[]]</r>
;; </search>


;; => can put code in org files???

;; auto updating pages - update on load



