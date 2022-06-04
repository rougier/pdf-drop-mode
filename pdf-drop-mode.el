;;; pdf-drop-mode.el --- Get DOI from PDF files dropped onto a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/pdf-drop-mode
;; Keywords: convenience
;; Version: 0.1.0

;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Documentation

;;; pdf-drop-mode is a convenient mode that search for the DOI of any file that
;;; is dropped onto a buffer. To do that, the mode search the DOI using several
;;; different methods (whose order can be specified). By default, it will first
;;; look at the file metadata to check if a DOI is attached using exiftool
;;; (whose path can be specified). If this fails, it will search the content of
;;; the file to try to locate a DOI regex on the first page (PDF is transformed
;;; into text using the pdftotext utility). If this fails, it will search again
;;; the metadata to try to find the title and query the crossref database to get
;;; a corresponding DOI. If this fails, it will ask the user to enter the title
;;; of the file and query the crossref database to try to get a correspondong
;;; DOI. If this fails, then the search has failed. When search finishes, a
;;; used-defined hook is ran with the filename and the DOI as arguments.

;;; To Do;
;;;
;;; - Handle arXiv papers as well. 
;;; - Better error identification and handling

;;; News

;; Version 0.1.0
;; Initial release

;;; Code:
(require 'bibtex)

(defcustom pdf-drop-pdftotext-path "pdftotext"
  "Path to the pdftotext executable (used to extract DOI from the
content of a pdf)."
  :type 'file)

(defcustom pdf-drop-exiftool-path "exiftool"
  "Path to the exiftool executable (used to extract TITLE from
pdf metadata)."
  :type 'file)

(defcustom pdf-drop-search-methods '(metadata content title user-title)
  "Ordered list of methods to use to get the DOI from a pdf."
  :type 'list)

(defcustom pdf-drop-search-hook nil
  "A pointer to a functi<on that is called when a search ends.
Called function is provided with the filename and the associated
doi (or nil) as arguments."
  :type 'function)

(defvar pdf-drop--doi-regex "\\(10\\.[0-9]\\{4,9\\}/[-+._;()/:A-Z0-9]\\{4,\\}[-+_()/:A-Z0-9]\\)"
  "Regular expressions matching a DOI.")

(defvar pdf-drop--crossref-search-url "http://search.crossref.org/dois?q=%s&rows=10"
  "URL to the crossref search API")

(defvar pdf-drop--crossref-query-url "http://dx.doi.org/%s"
  "URL to the crossref query API")

;; Adapted from https://github.com/jkitchin/org-ref (doi-utils.el)
;; Copyright (C) 2015-2021 John Kitchin (GPL v2)
(defun pdf-drop-get-doi-from-title (title &optional score-threshold)
  "Query crossref.org for TITLE and return the corresponding doi.

When multiple canditates are found, the one with the smallest
levenshtein distance is chosen if its distance to TITLE is less
than SCORE-THRESHOLD. Else, user is asked to choose among 5 best
candidates. If no SCORE-THRESHOLD is given, the first candidate
doi is returned."

  (interactive "sTitle: ")
  (with-current-buffer (url-retrieve-synchronously
                        (format
                         pdf-drop--crossref-search-url
                         (url-hexify-string title)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<i>\\|</i>" nil t)
      	(replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "&amp;" nil t)
	    (replace-match "&"))
      (goto-char (point-min))
      (while (re-search-forward "&quot;" nil t)
      	(replace-match "\\\"" nil t)))
    (setq raw-json-string
          (buffer-substring url-http-end-of-headers (point-max)))
    (setq json-string
          (decode-coding-string (encode-coding-string raw-json-string 'utf-8) 'utf-8))
    (setq json-data
          (json-read-from-string json-string))
    
    (let* (;; (score-threshold (or score-threshold 10))
           (candidates (mapcar (lambda (x)
                                 (let* ((x-full (cdr (assoc 'fullCitation x)))
                                        (x-title (cdr (assoc 'title x)))
                                        (x-display-title (if (string-match "\\([0-9]\\{4\\}\\)" x-full)
                                                             (format "%s (%s)" x-title (match-string 1 x-full))
                                                           (format "%s (????)" x-title)))
                                        (x-doi (cdr (assoc 'doi x))))
                                   (cons x-display-title  x-doi)))
                               json-data))
           (sorted-candidates (sort candidates
                                    #'(lambda (item-1 item-2)
                                        (let ((title-1 (substring (car item-1) 0 -7))
                                              (title-2 (substring (car item-2) 0 -7)))
                                          (< (string-distance title title-1)
                                             (string-distance title title-2))))))
           (best-match (caar sorted-candidates))
           (best-score (string-distance title (substring best-match 0 -7)))
           (selected (if (or (= (length sorted-candidates) 1)
                             (not score-threshold)
                             (< best-score score-threshold))
                         best-match
                       (completing-read "Matched title: " sorted-candidates nil t
                                        (if (< best-score 10) best-match nil))))
	       (doi (cdr (assoc selected sorted-candidates))))
      doi)))

(defun pdf-drop-get-doi-from-content (file)
  "Extract doi from a pdf FILE content (after conversion to text)."

  (unwind-protect
      (with-current-buffer (get-buffer-create shell-command-buffer-name)
        (shell-command (format "%s -f 1 -l 1 -enc UTF-8 %s -"
                               pdf-drop-pdftotext-path
                           (shell-quote-argument file)))
        (goto-char (point-min))
        (if (search-forward-regexp pdf-drop--doi-regex nil t)
            (match-string 1)))
    (kill-buffer shell-command-buffer-name)))

(defun pdf-drop-get-tag-from-metadata (file tag)
  "Get TAG from file metadata."

  (let ((output (string-trim
                 (shell-command-to-string
                  (format "%s -s -s -s -%s %s"
                          pdf-drop-exiftool-path
                          tag
                          (shell-quote-argument file))))))
    (if (> (length output) 0)
        output
      nil)))

(defun pdf-drop-get-doi-from-metadata (file)
  "Get DOI from file metadata."
  
  (pdf-drop-get-tag-from-metadata file "DOI"))

(defun pdf-drop-get-title-from-metadata (file)
  "Get TITLE from file metadata."
  
  (pdf-drop-get-tag-from-metadata file "TITLE"))


(defun pdf-drop--file-dnd-fallback (uri action)
  (let ((dnd-protocol-alist
         (rassq-delete-all 'pdf-drop--file-dnd-protocol
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun pdf-drop--file-dnd-protocol (uri action)
  ;; (condition-case nil
  ;;      (pdf-drop--process (substring uri 7))
  ;;    (error
  ;;     (pdf-drop--file-dnd-fallback uri action))))
  (let ((file (dnd-get-local-file-name uri)))
    (pdf-drop--process file)))

(defun pdf-drop--process (file)
  "Try to get the DOI associated to file."
  
  (let ((doi))
    (catch 'found
      (dolist (method pdf-drop-search-methods)
        
        (cond ((eq method 'content) 
               (setq doi (pdf-drop-get-doi-from-content file))
               (when doi (throw 'found doi)))

              ((eq method 'metadata)
               (setq doi (pdf-drop-get-doi-from-content file))
               (when doi (throw 'found doi)))
            
              ((eq method 'title)
               (let ((title (pdf-drop-get-title-from-metadata file)))
                 (when (and title (> (length title) 0))
                   (setq doi (pdf-drop-get-doi-from-title title))
                   (when doi (throw 'found doi)))))

              ((eq method 'user-title)
               (let ((buffer (find-file file)))
                 (with-current-buffer buffer
                   (unwind-protect
                       (let* ((title (read-string
                                      (format "Enter title for %s: "
                                              (file-name-nondirectory file)))))
                         (setq doi (pdf-drop-get-doi-from-title title 5))
                         (when doi (throw 'found doi)))
                     (kill-buffer buffer))))))))

    ;; Debug
    (when (and doi pdf-drop-search-hook)
      (apply pdf-drop-search-hook (list file doi)))))


(defun pdf-drop--bibtex-from-doi (doi)
  "Retrieve bibtex item using crossref information and DOI."
  
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
      (url-retrieve-synchronously (format pdf-drop--crossref-query-url doi))
      (setq bibtex-entry (buffer-substring 
          	              (string-match "@" (buffer-string))
                          (point)))
      (kill-buffer (current-buffer))))
  (with-temp-buffer
    (insert (decode-coding-string bibtex-entry 'utf-8))
    (goto-char (point-min))
    (bibtex-set-dialect 'biblatex t)
    (let ((bibtex-autokey-edit-before-use nil)
          (bibtex-entry-format nil))
      (bibtex-clean-entry t))
    (bibtex-reformat)
    (buffer-substring (point-min) (point-max))))

(define-minor-mode pdf-drop-mode
  "Minor mode that search for the doi of any pdf file dropped onto a buffer."

  :global t
  :init-value nil

  (let ((item '("^file:" . pdf-drop--file-dnd-protocol)))
    (if pdf-drop-mode
        (add-to-list 'dnd-protocol-alist item)
      (setq dnd-protocol-alist (remove item dnd-protocol-alist)))))
 

(provide 'pdf-drop-mode)
;;; pdf-drop-mode.el ends here

