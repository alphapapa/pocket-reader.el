;;; pocket-reader.el --- Client for Pocket reading list -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net
;; Created: 2017-09-25
;; Version: 0.1-pre
;; Keywords: pocket
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (kv "0.0.19") (pocket-lib "0.1") (s "1.10") (ov "1.0.6") (rainbow-identifiers "0.2.2") (org-web-tools "0.1"))
;; URL: https://github.com/alphapapa/pocket-reader.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a client for Pocket (getpocket.com).  It allows you to
;; manage your reading list: add, remove, delete, tag, view, favorite,
;; etc.  Doing so in Emacs with the keyboard is fast and efficient.
;; Links can be opened in Emacs with any function, or in external
;; browsers, and specific sites/URLs can be opened with specific
;; browser functions.  Views can be sorted by date, title, domain,
;; tags, favorite, etc, and "limited" mutt-style.  Items can be
;; searched for using keywords, tags, favorite status, unread/archived
;; status, etc.
;;
;; These keys can be used in the pocket-reader buffer:
;;
;; "RET" pocket-reader-open-url
;; "TAB" pocket-reader-pop-to-url
;; "a" pocket-reader-toggle-archived
;; "b" pocket-reader-open-in-external-browser
;; "c" pocket-reader-copy-url
;; "D" pocket-reader-delete
;; "e" pocket-reader-excerpt
;; "E" pocket-reader-excerpt-all
;; "*" pocket-reader-toggle-favorite
;; "f" pocket-reader-toggle-favorite
;; "F" pocket-reader-show-unread-favorites
;; "s" pocket-reader-search
;; "m" pocket-reader-toggle-mark
;; "M" pocket-reader-mark-all
;; "U" pocket-reader-unmark-all
;; "o" pocket-reader-more
;; "l" pocket-reader-limit
;; "tt" pocket-reader-add-tags
;; "ta" pocket-reader-add-tags
;; "tr" pocket-reader-remove-tags
;; "ts" pocket-reader-set-tags

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'url-parse)
(require 'seq)

(require 'dash)
(require 'kv)
(require 'ov)
(require 's)
(require 'rainbow-identifiers)

(require 'org-web-tools)
(require 'pocket-lib)

;;;; Variables

(defvar pocket-reader-mode-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "RET" pocket-reader-open-url
                    "TAB" pocket-reader-pop-to-url
                    "a" pocket-reader-toggle-archived
                    "b" pocket-reader-open-in-external-browser
                    "c" pocket-reader-copy-url
                    "D" pocket-reader-delete
                    "e" pocket-reader-excerpt
                    "E" pocket-reader-excerpt-all
                    "*" pocket-reader-toggle-favorite
                    "f" pocket-reader-toggle-favorite
                    "F" pocket-reader-show-unread-favorites
                    "s" pocket-reader-search
                    "m" pocket-reader-toggle-mark
                    "M" pocket-reader-mark-all
                    "U" pocket-reader-unmark-all
                    "o" pocket-reader-more
                    "l" pocket-reader-limit
                    "tt" pocket-reader-add-tags
                    "ta" pocket-reader-add-tags
                    "tr" pocket-reader-remove-tags
                    "ts" pocket-reader-set-tags
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map))

(defvar pocket-reader-items nil
  "Items to be shown.
This is stored in a var so we can fetch the items and calculate
settings for tabulated-list-mode based on it.  NOTE: This may
become out-of-sync with `tabulated-list-entries', so it should
not be used outside of functions that already use it.")

(defvar pocket-reader-offset 0
  "The current offset.")

(defvar pocket-reader-query nil
  "The current query string.")

(defvar pocket-reader-mark-overlays nil
  "List of overlays used to mark items.
Each item in the list is a cons cell whose first element is the
item ID and second is the overlay used to mark it.")

(defconst pocket-reader-keys
  '(:item_id
    :status
    :favorite
    (:tags . pocket-lib--process-tags)
    :time_added
    :time_updated
    :time_read
    :given_title
    :resolved_title
    :excerpt
    :has_video
    :has_image
    :word_count
    :amp_url
    :resolved_url)
  "Keys to use in Pocket API responses, optionally with function to filter each one through.")

;;;;; Customization

(defgroup pocket-reader nil
  "Library for accessing GetPocket.com API."
  :group 'external)

(defcustom pocket-reader-open-url-default-function
  #'org-web-tools-read-url-as-org
  "Default function to open items."
  :type 'function)

(defcustom pocket-reader-pop-to-url-default-function
  (lambda (url)
    (funcall #'org-web-tools-read-url-as-org url :show-buffer-fn #'pop-to-buffer))
  "Default function to pop-to items."
  :type 'function)

(defcustom pocket-reader-archive-on-open t
  "Mark items as read when opened."
  :type 'boolean)

(defcustom pocket-reader-color-site t
  "Colorize site names uniquely."
  :type 'boolean)

(defcustom pocket-reader-color-title t
  "Colorize titles according to site."
  :type 'boolean)

(defcustom pocket-reader-show-count 50
  "Show this many items in the list."
  :type 'integer)

(defcustom pocket-reader-url-open-fn-map
  '((eww-browse-url "news.ycombinator.com"))
  "A list mapping URL-matching regular expressions to functions used to open the URL.
Regexps are anchored after the protocol (i.e. \"https://\" is not
matched against).

This is useful when certain sites should be opened in an external
browser.  The list is backward in the sense that the functions
are listed first, followed by the regexps, in this format: (FN
REGEXP REGEXP ...)."
  :type '(alist :key-type function
                :value-type (repeat string)))

(defcustom pocket-reader-finalize-hook
  '(pocket-reader--apply-faces
    pocket-reader--add-spacers)
  "Functions run after printing items into the buffer."
  :type 'hook
  :options '(pocket-reader--apply-faces
             pocket-reader--add-spacers))

;;;;;; Faces

(defface pocket-reader-marked `((default :inverse-video t)) "Face for marked items")
(defface pocket-reader-unread `((default :weight bold)) "Face for unread items")
(defface pocket-reader-archived `((default :weight normal)) "Face for archived items")
(defface pocket-reader-favorite-star `((default :foreground "#b58900")) "Face for archived items")

;;;; Macros

(defmacro pocket-reader--with-pocket-reader-buffer (&rest body)
  "Run BODY in pocket-reader buffer and read-only inhibited."
  (declare (indent defun))
  `(with-current-buffer "*pocket-reader*"
     (let ((inhibit-read-only t))
       ,@body)))

(cl-defmacro pocket-reader--keywords-in-list (list &rest keywords)
  "If any KEYWORDS are in LIST, destructively remove them from LIST and return the last KEYWORD found in LIST."
  (declare (debug nil))
  `(car (last (cl-loop for keyword in ',keywords
                       when (member keyword ,list)
                       do (setq ,list (delete keyword ,list))
                       and collect (s-replace (rx ":") "" keyword)))))

(cl-defmacro pocket-reader--regexp-in-list (list regexp &optional (prefix ":"))
  "If REGEXP matches strings in LIST, destructively remove strings from LIST and return the last matching string without PREFIX."
  `(car (last (cl-loop for string in ,list
                       when (string-match ,regexp string)
                       do (setq ,list (delete string ,list))
                       and collect (replace-regexp-in-string (rx-to-string '(seq bos ,prefix)) "" string)))))

(defmacro pocket-reader--at-item (id-or-item &rest body)
  "Eval BODY with point at item ID-OR-ITEM.
If ID-OR-ITEM is an integer, convert it to a string.  If it's an
alist, get the `item-id' from it."
  (declare (indent defun) (debug (symbolp body)))
  `(pocket-reader--with-pocket-reader-buffer
     (let ((id (cl-typecase ,id-or-item
                 (list (number-to-string (alist-get 'item_id ,id-or-item)))
                 (integer (number-to-string ,id-or-item))
                 (string ,id-or-item))))
       (save-excursion
         (goto-char (point-min))
         (cl-loop while (not (eobp))
                  when (equal (tabulated-list-get-id) id)
                  return (progn
                           ,@body)
                  do (forward-line 1)
                  finally do (error "Item ID not found: %s" id))))))

(defmacro pocket-reader--at-marked-or-current-items (&rest body)
  "Execute BODY at each marked item, or current item if none are marked."
  (declare (indent defun))
  `(if pocket-reader-mark-overlays
       ;; Marked items
       (cl-loop for (id . ov) in pocket-reader-mark-overlays
                do (pocket-reader--at-item id
                     ,@body))
     ;; Current item
     ,@body))

;;;; Mode

(define-derived-mode pocket-reader-mode tabulated-list-mode
  "Pocket Reader"
  :group 'pocket-reader

  ;; FIXME: Unfortunately I can't get (local 'symbol) to work with
  ;; `advice-add', and I can't get `add-function' to work either, so I
  ;; have to use `advice-add', test the buffer each time the advice is
  ;; called, and delete the advice manually when the buffer is killed.
  (advice-add 'tabulated-list--sort-by-column-name :after 'pocket-reader--finalize)
  (add-hook 'kill-buffer-hook (lambda ()
                                (advice-remove 'tabulated-list--sort-by-column-name 'pocket-reader--finalize))
            'append 'local)

  (setq tabulated-list-sort-key '("Added" . nil))
  (pocket-reader-search)
  (unless (cdr tabulated-list-sort-key)
    ;; Invert initial sort order, putting most recent items on top
    (tabulated-list-sort 0)))

;;;; Functions

;;;;; Commands

(defun pocket-reader ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*pocket-reader*"))
  (pocket-reader-mode))

(defun pocket-reader-search (&optional query)
  "Search Pocket items with QUERY."
  ;; This function is the main one used to get and display items.
  ;; When QUERY is nil, it simply shows the default list of unread items.
  (interactive (list (read-from-minibuffer "Query: ")))
  (custom-reevaluate-setting 'pocket-reader-show-count)
  (pocket-reader-unmark-all)
  (setq pocket-reader-offset 0
        pocket-reader-query query
        pocket-reader-items nil)
  (let ((items (pocket-reader--get-items query)))
    (pocket-reader--add-items items)
    (unless items
      (message "No items for query: %s" query))))

(defun pocket-reader-show-unread-favorites ()
  "Show unread favorite items."
  (interactive)
  (pocket-reader-search ":* :unread"))

(defun pocket-reader-more (count)
  "Fetch and show COUNT more items."
  (interactive "p")
  (let* ((count (if (= 1 count)
                    pocket-reader-show-count
                  count))
         (offset (incf pocket-reader-offset count)))
    (pocket-reader--add-items (pocket-reader--get-items pocket-reader-query))))

(defun pocket-reader-limit (query)
  "Limit display to items matching QUERY."
  (interactive (list (read-from-minibuffer "Query: ")))
  (if (s-present? query)
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (not (eobp))
                 unless (re-search-forward query (line-end-position) t)
                 do (ov (line-beginning-position) (1+ (line-end-position)) 'display "")
                 do (forward-line 1)))
    ;; No query; show all entries
    (ov-clear 'display "")))

(defun pocket-reader-excerpt ()
  "Show excerpt for marked or current items."
  (interactive)
  (pocket-reader--at-marked-or-current-items
    (let ((excerpt (pocket-reader--get-property :excerpt)))
      (unless (s-blank-str? excerpt)
        (let* ((start-col (1+ (cl-second (pocket-reader--column-data "Title"))))
               (prefix (s-repeat start-col " "))
               (width (- (window-text-width) start-col))
               (left-margin start-col)
               (string (concat prefix (s-trim (propertize (pocket-reader--wrap-string excerpt width)
                                                          'face 'default)) "\n")))
          ;; Hide or show excerpt
          (unless (cl-loop for ov in (ov-forwards)
                           when (equal string (ov-val ov 'before-string))
                           do (ov-reset ov)
                           and return t)
            ;; Excerpt not found; show it
            (ov (1+ (line-end-position)) (1+ (line-end-position))
                'before-string string)))))))

(defun pocket-reader-excerpt-all ()
  "Show all excerpts."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((first-excerpt (cl-loop while (not (eobp))
                                  for excerpt = (pocket-reader--get-property :excerpt)
                                  when excerpt
                                  return excerpt
                                  finally do (error "No excerpts found"))))
      ;; Search for overlay showing this excerpt
      (if (cl-loop for ov in (ov-forwards)
                   thereis (equal (ov-val ov 'before-string) first-excerpt))
          ;; Already shown; hide all excerpts
          (cl-loop initially do (goto-char (point-min))
                   for ov in (ov-forwards)
                   when (not (equal (ov-val ov 'before-string) "\n"))
                   do (ov-reset ov))
        ;; Not shown; show all excerpts
        (goto-char (point-min))
        (while (not (eobp))
          (pocket-reader-excerpt)
          (forward-line 1))))))

;;;;;; Marking

(defun pocket-reader-toggle-mark ()
  "Toggle mark on current item."
  (interactive)
  ;; Make sure item is visible
  (unless (pocket-reader--item-visible-p)
    (error "toggle-mark called on invisible item: %s" (tabulated-list-get-id)))
  (if (pocket-reader--item-marked-p)
      ;; Marked; unmark
      (pocket-reader--unmark-item (tabulated-list-get-id))
    ;; Unmarked; mark
    (pocket-reader--mark-current-item))
  (next-line 1))

(defun pocket-reader-mark-all ()
  "Mark all visible items."
  (interactive)
  (pocket-reader--with-pocket-reader-buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (not (eobp))
               when (pocket-reader--item-visible-p)
               do (pocket-reader--mark-current-item)
               do (forward-line 1)))))

(defun pocket-reader-unmark-all ()
  "Unmark all items."
  (interactive)
  (cl-loop for (id . ov) in pocket-reader-mark-overlays
           do (pocket-reader--unmark-item id)))

;;;;;; Tags

(defun pocket-reader-add-tags (tags)
  "Add tags to current item."
  (interactive (list (read-from-minibuffer "Tags: ")))
  (let* ((new-tags (s-split (rx (or space ",")) tags 'omit-nulls))
         (new-tags-string (s-join "," new-tags)))
    (when (and new-tags-string
               (apply #'pocket-lib--tags-action 'tags_add new-tags-string
                      (pocket-reader--marked-or-current-items)))
      ;; Tags added successfully
      (pocket-reader--at-marked-or-current-items
        (pocket-reader--add-tags new-tags)))))

(defun pocket-reader-remove-tags (remove-tags)
  "Remove tags from current item."
  ;; FIXME: Get all tags with a function.
  (interactive (list (completing-read "Tags: " (pocket-reader--get-property :tags))))
  (let* ((remove-tags (s-split (rx (or space ",")) remove-tags 'omit-nulls))
         (remove-tags-string (s-join "," remove-tags)))
    (when (and remove-tags-string
               (apply #'pocket-lib--tags-action 'tags_remove remove-tags-string
                      (pocket-reader--marked-or-current-items)))
      ;; Tags removed successfully
      (pocket-reader--at-marked-or-current-items
        (pocket-reader--remove-tags remove-tags)))))

(defun pocket-reader-set-tags (tags)
  "Set TAGS of current item."
  (interactive (list (read-from-minibuffer "Tags: ")))
  (pocket-reader--with-pocket-reader-buffer
    (let* ((tags (s-split (rx (or space ",")) tags 'omit-nulls))
           (tags-string (s-join "," tags)))
      (when (apply #'pocket-lib--tags-action 'tags_replace tags-string (pocket-reader--marked-or-current-items))
        ;; Tags replaced successfully
        (pocket-reader--at-marked-or-current-items
          (pocket-reader--set-tags tags))))))

;;;;;; URL-opening

(defun pocket-reader-open-url (&optional &key fn)
  "Open URL of current item with default function."
  (interactive)
  (pocket-reader--at-marked-or-current-items
    (let* ((url (pocket-reader--get-property :resolved_url))
           (fn (or fn (pocket-reader--map-url-open-fn url))))
      (when (funcall fn url)
        ;; Item opened successfully
        (when pocket-reader-archive-on-open
          (pocket-reader--with-pocket-reader-buffer
            (pocket-reader-toggle-archived)))))))

(defun pocket-reader-pop-to-url ()
  "Open URL of current item with default pop-to function."
  (interactive)
  (pocket-reader-open-url :fn #'pocket-reader-pop-to-url-default-function))

(defun pocket-reader-open-in-external-browser ()
  (interactive)
  (pocket-reader-open-url
   :fn (lambda (&rest args)
         (apply #'browse-url-default-browser args)
         ;; Return t because the browsing function may not return non-nil
         ;; when it succeeds, preventing the item from being archived
         t)))

(defun pocket-reader-copy-url ()
  "Copy URL of current item to kill-ring/clipboard."
  (interactive)
  (when-let ((url (pocket-reader--get-property :resolved_url)))
    (kill-new url)
    (message url)))

;;;;;; Other

(defun pocket-reader-delete ()
  "Delete current or marked items (with confirmation)."
  (interactive)
  (when (yes-or-no-p "Delete item(s)?")
    (apply #'pocket-reader--delete-items (pocket-reader--marked-or-current-items))))

(defun pocket-reader-toggle-favorite ()
  "Toggle current or marked items' favorite status."
  (interactive)
  (cl-loop for item in (pocket-reader--marked-or-current-items)
           if (pocket-reader--at-item item
                (pocket-reader--is-favorite))
           collect item into unfavorites
           else collect item into favorites
           finally do (when favorites
                        (apply #'pocket-reader--favorite-items favorites))
           finally do (when unfavorites
                        (apply #'pocket-reader--unfavorite-items unfavorites))))

(defun pocket-reader-toggle-archived ()
  "Toggle current or marked items' archived/unread status."
  (interactive)
  (cl-loop for item in (pocket-reader--marked-or-current-items)
           if (pocket-reader--at-item item
                (pocket-reader--is-archived))
           collect item into readds
           else collect item into archives
           finally do (when readds
                        (apply #'pocket-reader--readd-items readds))
           finally do (when archives
                        (apply #'pocket-reader--archive-items archives))))

;;;;; Helpers

(defun pocket-reader--item-visible-p ()
  "Return non-nil if current item is visible (i.e. not hidden by an overlay)."
  (cl-loop for ov in (overlays-at (line-beginning-position))
           never (string= "" (ov-val ov 'display))))

(defun pocket-reader--add-items (items)
  "Add and display ITEMS."
  (setq pocket-reader-items (append pocket-reader-items items))
  (pocket-reader--set-tabulated-list-format)
  (setq tabulated-list-entries pocket-reader-items)
  (tabulated-list-init-header)
  (tabulated-list-revert)
  (pocket-reader--finalize))

(defun pocket-reader--delete-items (&rest items)
  "Delete ITEMS.
Items should be a list of items as returned by
`pocket-reader--marked-or-current-items'."
  (when (apply #'pocket-lib-delete items)
    (cl-loop for item in items
             for id = (number-to-string (alist-get 'item_id item))
             do (progn
                  (pocket-reader--unmark-item id)
                  (setq pocket-reader-items (cl-remove id pocket-reader-items
                                                       :test #'string= :key #'car))
                  (setq tabulated-list-entries pocket-reader-items)
                  (pocket-reader--at-item id
                    (tabulated-list-delete-entry))))))

(defun pocket-reader--finalize (&rest ignore)
  "Finalize the buffer after adding or sorting items."
  ;; Because we have to add this function as advice to
  ;; `tabulated-list--sort-by-column-name', causing it to run in every
  ;; tabulated-list buffer, we must make sure it's the pocket-reader
  ;; buffer.
  (when (string= "*pocket-reader*" (buffer-name))
    (run-hooks 'pocket-reader-finalize-hook)))

(defun pocket-reader--get-items (&optional query)
  "Return Pocket items for QUERY.
QUERY is a string which may contain certain keywords:

:*, :favorite  Return only favorited items.
:archive       Return only archived items.
:unread        Return only unread items (default).
:all           Return all items.
:COUNT         Return at most COUNT (a number) items.
:t:TAG         Return items with TAG (only one tag may be searched for)."
  ;; This buffer-local variable specifies the entries displayed in the
  ;; Tabulated List buffer.  Its value should be either a list, or a
  ;; function.
  ;;
  ;; If the value is a list, each list element corresponds to one entry,
  ;; and should have the form ‘(ID CONTENTS)’, where
  ;;
  ;; • ID is either ‘nil’, or a Lisp object that identifies the
  ;; entry.  If the latter, the cursor stays on the same entry when
  ;; re-sorting entries.  Comparison is done with ‘equal’.
  ;;
  ;; • CONTENTS is a vector with the same number of elements as
  ;; ‘tabulated-list-format’.  Each vector element is either a
  ;;  string, which is inserted into the buffer as-is, or a list
  ;;  ‘(LABEL . PROPERTIES)’, which means to insert a text button by
  ;;   calling ‘insert-text-button’ with LABEL and PROPERTIES as
  ;;   arguments (*note Making Buttons::).
  ;;
  ;;   There should be no newlines in any of these strings.

  ;; FIXME: Add error handling.
  (let* ((query (or query ""))
         ;; Parse query
         (query-words (s-split " " query))
         (state (pocket-reader--keywords-in-list query-words ":archive" ":all" ":unread"))
         (favorite (when (pocket-reader--keywords-in-list query-words ":favorite" ":*") 1))
         (count (setq pocket-reader-show-count
                      (or (--when-let (pocket-reader--regexp-in-list query-words (rx bos ":" (1+ digit) eos))
                            (string-to-number it))
                          pocket-reader-show-count)))
         (tag (pocket-reader--regexp-in-list query-words (rx bos ":t:" (1+ word) eos) ":t:"))
         (query-string (s-join " " query-words))
         ;; Get items with query
         (items (cdr (cl-third (pocket-lib-get :detail-type "complete" :count count :offset pocket-reader-offset
                                 :search query-string :state state :favorite favorite :tag tag))))
         ;; Convert list of alists to plists with selected keys
         (item-plists (--map (cl-loop with item = (kvalist->plist (cdr it))
                                      for key in pocket-reader-keys
                                      for fn = nil
                                      when (consp key)
                                      do (setq fn (cdr key)
                                               key (car key))
                                      for val = (if fn
                                                    (funcall fn (plist-get item key))
                                                  (plist-get item key))
                                      when val
                                      append (list key val))
                             items)))
    ;; Collect data from plists and return as list of vectors for tabulated-list
    (cl-loop for it in item-plists
             for title = (pocket-reader--not-empty-string (apply #'propertize (pocket-reader--or-string-not-blank
                                                                               (plist-get it :resolved_title)
                                                                               (plist-get it :given_title)
                                                                               "[untitled]")
                                                                 (cl-loop for key in pocket-reader-keys
                                                                          when (consp key)
                                                                          do (setq key (car key))
                                                                          append (list key (plist-get it key)))))
             for tags = (pocket-reader--not-empty-string (s-join "," (plist-get it :tags)))
             collect (list (plist-get it :item_id)
                           (vector (pocket-reader--format-timestamp (string-to-number (plist-get it :time_added)))
                                   (pocket-reader--favorite-string (plist-get it :favorite))
                                   title
                                   (pocket-reader--url-domain (plist-get it :resolved_url))
                                   tags)))))

(defun pocket-reader--action (action &optional arg)
  "Execute ACTION on marked or current items.
ACTION should be a string or symbol which is the name of an
action in the Pocket API."
  ;; MAYBE: Not currently using this, may not need it.
  (pocket-reader--with-pocket-reader-buffer
    (apply #'pocket-lib--action action (pocket-reader--marked-or-current-items))))

(defun pocket-reader--marked-or-current-items ()
  "Return marked or current items, suitable for passing to `pocket-lib' functions."
  (or (cl-loop for (id . ov) in pocket-reader-mark-overlays
               collect (list (cons 'item_id (string-to-number id))))
      (list (pocket-reader--current-item))))

(defun pocket-reader--set-tabulated-list-format ()
  "Set `tabulated-list-format' according to the maximum width of items about to be displayed."
  (when-let ((site-width (cl-loop for item in pocket-reader-items
                                  maximizing (length (elt (cadr item) 3))))
             (title-width (- (window-text-width) 11 2 site-width 10 1)))
    (setq tabulated-list-format (vector (list "Added" 10 t)
                                        (list "*" 1 t)
                                        (list "Title" title-width t)
                                        (list "Site" site-width t)
                                        (list "Tags" 10 t)))))

(defun pocket-reader--map-url-open-fn (url)
  "Return function to use to open URL.
Checks `pocket-reader-url-open-fn-map' for a function to use.  If
none is found, returns `pocket-reader-open-url-default-function'."
  (or (car (cl-rassoc url pocket-reader-url-open-fn-map
                      :test (lambda (url regexp)
                              (string-match (rx-to-string `(seq "http" (optional "s") "://"
                                                                (regexp ,(car regexp))
                                                                (or "/" eos)))
                                            url))))
      pocket-reader-open-url-default-function))

(defun pocket-reader--current-item ()
  "Return list containing cons of current item's ID, suitable for passing to pocket-lib."
  (let* ((id (string-to-number (tabulated-list-get-id))))
    (list (cons 'item_id id))))

(defun pocket-reader--get-property (property)
  "Return value of PROPERTY for current item."
  (get-text-property 0 property (elt (tabulated-list-get-entry) 2)))

(defun pocket-reader--set-property (property value)
  "Set current item's PROPERTY to VALUE."
  ;; Properties are stored in the title column
  (pocket-reader--with-pocket-reader-buffer
    (let ((title (elt (tabulated-list-get-entry) 2)))
      (put-text-property 0 (length title)
                         property value
                         title)
      (tabulated-list-set-col 2 title))))

(defun pocket-reader--url-domain (url)
  "Return domain for URL.
Common prefixes like www are removed."
  (replace-regexp-in-string (rx bos (and (or "www") ".")) ""
                            (url-host (url-generic-parse-url url))))

(defun pocket-reader--format-timestamp (timestamp)
  "Format TIMESTAMP."
  (format-time-string "%Y-%m-%d" timestamp))

(defun pocket-reader--add-spacers (&rest ignore)
  "Insert overlay spacers where the current sort column's values change.
For example, if sorted by date, a spacer will be inserted where
the date changes."
  (let ((sort-column (seq-position tabulated-list-format tabulated-list-sort-key
                                   (lambda (seq elt)
                                     (string= (car seq) (car elt))))))
    ;; Clear existing spacers
    (ov-clear)
    (save-excursion
      (goto-char (point-min))
      (cl-loop with prev-data = (elt (tabulated-list-get-entry) sort-column)
               while (not (eobp))
               do (forward-line 1)
               for current-data = (elt (tabulated-list-get-entry) sort-column)
               when (not (equal current-data prev-data))
               do (progn
                    (ov (line-beginning-position) (line-beginning-position) 'before-string "\n")
                    (setq prev-data current-data))))))

;;;;;; Archived/readd

(defun pocket-reader--archive-items (&rest items)
  "Mark ITEMS as archived."
  (when (apply #'pocket-lib-archive items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property :status "1")
             (pocket-reader--apply-faces-to-line))
           items)))

(defun pocket-reader--readd-items (&rest items)
  "Readd ITEMS."
  (when (apply #'pocket-lib-readd items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property :status "0")
             (pocket-reader--apply-faces-to-line))
           items)))

(defun pocket-reader--is-archived ()
  "Return non-nil if current item is archived."
  (string= "1" (pocket-reader--get-property :status)))

;;;;;; Favorites

(defun pocket-reader--favorite-items (&rest items)
  "Mark ITEMS as favorites."
  (when (apply #'pocket-lib-favorite items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property :favorite "1")
             (pocket-reader--update-favorite-display t))
           items)))

(defun pocket-reader--unfavorite-items (&rest items)
  "Unmark ITEMS as favorites."
  (when (apply #'pocket-lib-unfavorite items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property :favorite "0")
             (pocket-reader--update-favorite-display nil))
           items)))

(defun pocket-reader--is-favorite ()
  "Return non-nil if current item is a favorite."
  (string= "1" (pocket-reader--get-property :favorite)))

(defun pocket-reader--update-favorite-display (is-favorite)
  "Update favorite star for current item."
  (tabulated-list-set-col 1 (if is-favorite "*" "") t)
  (pocket-reader--apply-faces-to-line))

;;;;;; Tags

(defun pocket-reader--add-tags (tags)
  "Add TAGS to current item.
TAGS should be a list of strings."
  (let* ((old-tags (pocket-reader--get-property :tags))
         (new-tags (append old-tags tags)))
    (pocket-reader--set-tags new-tags)))

(defun pocket-reader--remove-tags (tags)
  "Remove TAGS from current item.
TAGS should be a list of strings."
  (let* ((old-tags (pocket-reader--get-property :tags))
         (new-tags (seq-difference old-tags tags #'string=)))
    (pocket-reader--set-tags new-tags)))

(defun pocket-reader--set-tags (tags)
  "Set current item's tags to TAGS.
TAGS should be a list of strings.  Tags are sorted and
deduplicated."
  (let* ((tags (-sort #'string< (-uniq tags))))
    (pocket-reader--set-property :tags tags)
    (pocket-reader--set-tags-column)
    (pocket-reader--apply-faces-to-line)))

(defun pocket-reader--set-tags-column ()
  "Set tags column for current entry.
Gets tags from text property."
  (tabulated-list-set-col 4 (s-join "," (pocket-reader--get-property :tags))))

;;;;;; Marking

(defun pocket-reader--mark-current-item ()
  "Mark current item."
  (unless (pocket-reader--item-marked-p)
    (push (cons (tabulated-list-get-id) (ov (line-beginning-position) (line-end-position)
                                            'face 'pocket-reader-marked))
          pocket-reader-mark-overlays)))

(defun pocket-reader--unmark-item (id)
  "Unmark item by ID."
  (let ((ov (alist-get id pocket-reader-mark-overlays)))
    (ov-reset ov))
  (setq pocket-reader-mark-overlays (cl-remove id pocket-reader-mark-overlays :test #'string= :key #'car)))

(defun pocket-reader--item-marked-p ()
  "Return non-nil if current item is marked."
  (let ((id (tabulated-list-get-id)))
    (cl-member id pocket-reader-mark-overlays
               :test #'string= :key #'car)))

;;;;;; Strings

(defun pocket-reader--favorite-string (val)
  "If VAL is 1, return the star character as a string, otherwise the empty string."
  (pcase val
    ("0" "")
    ("1" "*")))

(defun pocket-reader--wrap-string (string length)
  "Wrap STRING to LENGTH."
  (if (<= (length string) length)
      string
    (s-trim (with-temp-buffer
              (insert string)
              (let ((fill-column length))
                (fill-region-as-paragraph (point-min) (point-max))
                (buffer-string))))))

(defun pocket-reader--not-empty-string (s)
  "If S is non-empty, return it; otherwise return \" \"."
  ;; No column may be actually empty, because `tabulated-list-set-col' doesn't work on
  ;; nil columns, because it uses `next-single-property-change' to find the place to
  ;; modify.  So we use an empty string instead of nil.
  (if (string-empty-p s)
      " "
    s))

(defun pocket-reader--or-string-not-blank (&rest strings)
  "Return first non-empty string in STRINGS."
  (cl-loop for string in strings
           when (and string (not (s-blank-str? string)))
           return string))

;;;;;; Faces

(defun pocket-reader--apply-faces ()
  ;; TODO: Maybe we should use a custom print function but this is simpler
  (pocket-reader--with-pocket-reader-buffer
    (goto-char (point-min))
    (while (not (eobp))
      (pocket-reader--apply-faces-to-line)
      (forward-line 1))
    (goto-char (point-min))))

(defun pocket-reader--apply-faces-to-line ()
  "Apply faces to current line."
  (pocket-reader--with-pocket-reader-buffer
    (add-text-properties (line-beginning-position) (line-end-position)
                         (list 'face (pcase (pocket-reader--get-property :status)
                                       ("0" 'pocket-reader-unread)
                                       ("1" 'pocket-reader-archived)) ))
    (when (pocket-reader--get-property :favorite)
      (pocket-reader--set-column-face "*" 'pocket-reader-favorite-star))
    (when (or pocket-reader-color-site
              pocket-reader-color-title)
      (pocket-reader--set-site-face))))

(defun pocket-reader--set-site-face ()
  "Apply colored face to site column for current entry."
  (let* ((column (tabulated-list--column-number "Site"))
         (site (elt (tabulated-list-get-entry) column))
         (hash (rainbow-identifiers--hash-function site))
         (face (rainbow-identifiers-cie-l*a*b*-choose-face hash)))
    (when pocket-reader-color-site
      (pocket-reader--set-column-face "Site" face))
    (when pocket-reader-color-title
      (pocket-reader--set-column-face "Title" face))))

(defun pocket-reader--set-column-face (column face)
  "Apply FACE to COLUMN on current line.
COLUMN may be the column name or number."
  (-let* (((num start end width) (pocket-reader--column-data column))
          ;; Convert column positions to buffer positions
          (start (+ (line-beginning-position) start))
          (end (+ start width)))
    (pocket-reader--with-pocket-reader-buffer
      (add-face-text-property start end face t))))

(defun pocket-reader--column-data (column)
  "Return data about COLUMN.
COLUMN may be a number or the heading string.

Returns list with these values:

- Column number (if COLUMN is a string)
- Start column (column on each line that COLUMN starts at)
- End column (column on each line that COLUMN stops at)
- Column width (in characters)"
  (let* ((col-num (cl-typecase column
                    (integer column)
                    (string (tabulated-list--column-number column))))
         (col-data (aref tabulated-list-format col-num))
         (start-col (1+ (cl-loop for i from 0 below col-num
                                 for col-data = (aref tabulated-list-format i)
                                 for col-width = (elt col-data 1)
                                 sum col-width)))
         (column-width (elt col-data 1))
         (end-col (+ start-col column-width)))
    (list col-num start-col end-col column-width)))

;;;; Footer

(provide 'pocket-reader)

;;; pocket-reader.el ends here
