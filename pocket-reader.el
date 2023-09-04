;;; pocket-reader.el --- Client for Pocket reading list -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Created: 2017-09-25
;; Version: 0.3
;; Keywords: pocket
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (kv "0.0.19") (peg "1.0.1") (pocket-lib "0.1") (s "1.10") (ov "1.0.6") (rainbow-identifiers "0.2.2") (org-web-tools "0.1") (ht "2.2"))
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
;; "d" pocket-reader (return to default view)
;; "D" pocket-reader-delete
;; "e" pocket-reader-excerpt
;; "E" pocket-reader-excerpt-all
;; "*" pocket-reader-toggle-favorite
;; "f" pocket-reader-toggle-favorite
;; "F" pocket-reader-show-unread-favorites
;; "g" pocket-reader-resort
;; "G" pocket-reader-refresh
;; "s" pocket-reader-search
;; "m" pocket-reader-toggle-mark
;; "M" pocket-reader-mark-all
;; "U" pocket-reader-unmark-all
;; "o" pocket-reader-more
;; "l" pocket-reader-limit
;; "R" pocket-reader-random-item
;; "ta" pocket-reader-add-tags
;; "tr" pocket-reader-remove-tags
;; "tt" pocket-reader-set-tags
;; "ts" pocket-reader-tag-search
;;
;; In eww, Org, w3m, and some other major modes,
;; `pocket-reader-add-link' can be used to add a link at point to
;; Pocket.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'url-parse)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)

(require 'dash)
(require 'kv)
(require 'ht)
(require 'ov)
(require 'peg)
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
                    "d" pocket-reader ; Return to default view
                    "D" pocket-reader-delete
                    "e" pocket-reader-excerpt
                    "E" pocket-reader-excerpt-all
                    "*" pocket-reader-toggle-favorite
                    "f" pocket-reader-toggle-favorite
                    "F" pocket-reader-show-unread-favorites
                    "g" pocket-reader-resort
                    "G" pocket-reader-refresh
                    "s" pocket-reader-search
                    "m" pocket-reader-toggle-mark
                    "M" pocket-reader-mark-all
                    "u" pocket-reader-unmark-all
                    "o" pocket-reader-more
                    "l" pocket-reader-limit
                    "R" pocket-reader-random-item
                    "ta" pocket-reader-add-tags
                    "tr" pocket-reader-remove-tags
                    "tt" pocket-reader-set-tags
                    "ts" pocket-reader-tag-search
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map))

(defvar pocket-reader-items nil
  "Items to be shown.
This is stored in a var so we can fetch the items and calculate
settings for ‘tabulated-list-mode’ based on it.  NOTE: This may
become out-of-sync with `tabulated-list-entries', so it should
not be used outside of functions that already use it.")

(defvar pocket-reader-offset 0
  "The current offset.")

(defvar pocket-reader-queries nil
  "List of current query strings.")

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
    :given_url
    :amp_url
    :resolved_url)
  "Keys to use in Pocket API responses.
Each item may also be a cons cell in which the cdr is a function
to filter each one through.")

;;;;; Customization

(defgroup pocket-reader nil
  "Library for accessing GetPocket.com API."
  :group 'external)

(defcustom pocket-reader-default-queries nil
  "Default queries, used for initial view."
  :type '(repeat string))

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

(defcustom pocket-reader-site-column-max-width 22
  "Maximum width of the site column."
  :type 'integer)

(defcustom pocket-reader-url-open-fn-map
  '((eww-browse-url "news.ycombinator.com"))
  ;; FIXME: This is supposed to be an alist, but the default value
  ;; isn't one.
  "List mapping URL-matching regexps to functions used to open the URL.
Regexps are anchored after the protocol (i.e. \"https://\" is not
matched against).

This is useful when certain sites should be opened in an external
browser.  The list is backward in the sense that the functions
are listed first, followed by the regexps, in this format: (FN
REGEXP REGEXP ...)."
  :type '(alist :key-type function
                :value-type (repeat string)))

(defcustom pocket-reader-domain-url-type-map
  '((resolved_url "reddit.com"))
  "A list mapping URL types from `pocket-reader-url-priorities' to domains.

This is useful when certain sites should have certain URL types
preferred (e.g. if you prefer not to load AMP URLs for Reddit)."
  :type '(alist :key-type symbol
                :value-type (repeat string)))

(defcustom pocket-reader-finalize-hook
  '(pocket-reader--apply-faces
    pocket-reader--add-spacers)
  "Functions run after printing items into the buffer."
  :type 'hook
  :options '(pocket-reader--apply-faces
             pocket-reader--add-spacers))

(defcustom pocket-reader-url-priorities
  '(amp_url resolved_url given_url)
  "URLs for each item are chosen in this order.
Pocket provides multiple URLs for each item, depending on what it
can find.  This allows users to choose which URLs they prefer to
use when opening, copying, etc."
  :type '(repeat symbol)
  :options '(amp_url resolved_url given_url))

(defcustom pocket-reader-added-column-sort-function #'pocket-reader--added-fancy<
  "Function to sort the \"Added\" column."
  :type '(radio (function-item :tag "Default (by date, then favorite, then tags, then domain)" pocket-reader--added-fancy<)
                (function-item :tag "By date only" pocket-reader--added<)
                (function :tag "Custom function")))

;;;;;; Faces

(defface pocket-reader-marked `((default :inverse-video t)) "Face for marked items")
(defface pocket-reader-unread `((default :weight bold)) "Face for unread items")
(defface pocket-reader-archived `((default :weight normal)) "Face for archived items")
(defface pocket-reader-favorite-star `((default :foreground "#b58900")) "Face for favorite items")

;;;; Macros

(defmacro pocket-reader--with-pocket-reader-buffer (&rest body)
  "Run BODY in ‘pocket-reader’ buffer and read-only inhibited."
  (declare (indent defun))
  `(with-current-buffer "*pocket-reader*"
     (let ((inhibit-read-only t))
       ,@body)))

(cl-defmacro pocket-reader--keywords-in-list (list &rest keywords)
  "Destructively remove KEYWORDS from LIST and return the last keyword found."
  (declare (debug nil))
  `(car (last (cl-loop for keyword in ',keywords
                       when (member keyword ,list)
                       do (setq ,list (delete keyword ,list))
                       and collect (s-replace (rx ":") "" keyword)))))

(cl-defmacro pocket-reader--regexp-in-list (list regexp &optional (prefix ":"))
  "Return last match of REGEXP in LIST, without PREFIX.
Also destructively removes matching strings from LIST."
  `(car (last (cl-loop for string in ,list
                       when (string-match ,regexp string)
                       do (setq ,list (delete string ,list))
                       and collect (replace-regexp-in-string (rx-to-string `(seq bos (regexp ,,prefix))) "" string)))))

(defmacro pocket-reader--at-item (id-or-item &rest body)
  "Eval BODY with point at item ID-OR-ITEM.
ID-OR-ITEM should be an integer or an alist.  If it's an alist,
get the `item-id' from it."
  (declare (indent defun) (debug (symbolp body)))
  `(pocket-reader--with-pocket-reader-buffer
     (let ((id (cl-typecase ,id-or-item
                 (integer ,id-or-item)
                 (list (alist-get 'item_id ,id-or-item)))))
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
  (setq pocket-reader-queries pocket-reader-default-queries)
  (pocket-reader-refresh)
  (unless (cdr tabulated-list-sort-key)
    ;; Invert initial sort order, putting most recent items on top
    (tabulated-list-sort 0)))

;;;; Functions

;;;;; Commands

;;;###autoload
(defun pocket-reader ()
  "Show Pocket reading list."
  (interactive)
  (switch-to-buffer (get-buffer-create "*pocket-reader*"))
  (pocket-reader-mode))

(cl-defun pocket-reader-search (&optional query &key add)
  "Search Pocket items with QUERY.
If QUERY is nil, show default list.  With prefix or ADD non-nil,
add items instead of replacing."
  ;; This function is the main one used to get and display items.
  (interactive (list (read-from-minibuffer "Query: ")))
  (unless (or current-prefix-arg add)
    ;; Not adding; reset everything
    (goto-char (point-min))
    (custom-reevaluate-setting 'pocket-reader-show-count)
    (pocket-reader-unmark-all)
    (setq pocket-reader-offset 0
          pocket-reader-queries nil
          pocket-reader-items (ht)))
  (let ((items (pocket-reader--get-items query)))
    (if items
        (progn
          (cl-pushnew query pocket-reader-queries :test #'string=)
          (pocket-reader--add-items items))
      ;; No items found
      (cl-case pocket-reader-offset
        (0 (message "No items for query: %s" query))
        (t (message "No more items for query: %s" query))))))

(defun pocket-reader-refresh ()
  "Refresh list using current queries."
  (interactive)
  ;; TODO: Can we use the API's "since" option to just get changes?
  (let ((first-line-visible-p (pos-visible-in-window-p (point-min))))
    (cl-case (length pocket-reader-queries)
      (1 (pocket-reader-search (car pocket-reader-queries)))
      (t (let ((queries (cdr pocket-reader-queries)))
           ;; Run the first query as a replacing search, then the rest
           ;; as adding ones.  We save the queries, because the
           ;; replacing search overwrites them.
           (pocket-reader-search (car pocket-reader-queries))
           (--each queries
             (pocket-reader-search it :add t)))))
    (when first-line-visible-p
      ;; If point is on the first item, and new items are added above
      ;; it, the new items will be off-screen, and the user won't
      ;; realize they have been added.  So, if we started on what was
      ;; the first line, show what's now the first line.
      (let ((pos (point)))
        (goto-char (point-min))
        (redisplay)
        (goto-char pos)))))

(defun pocket-reader-show-unread-favorites ()
  "Show unread favorite items."
  (interactive)
  (pocket-reader-search ":* :unread"))

(defun pocket-reader-more (count)
  "Fetch and show COUNT more items."
  (interactive "p")
  (let* ((count (if (= 1 count)
                    pocket-reader-show-count
                  count)))
    (cl-incf pocket-reader-offset count)
    (--each pocket-reader-queries
      (pocket-reader-search it :add t))))

(defun pocket-reader-limit (query)
  "Limit display to items matching QUERY."
  ;; MAYBE: Search hidden properties so e.g. the URL can be matched against.
  (interactive (list (read-from-minibuffer "Query: ")))
  (if (s-present? query)
      (save-excursion
        (pocket-reader-unmark-all)
        (goto-char (point-min))
        (while (not (eobp))
          (unless (re-search-forward query (line-end-position) t)
            (ov (line-beginning-position) (1+ (line-end-position)) 'display ""))
          (forward-line 1)))
    ;; No query; show all entries
    (ov-clear 'display "")))

(defun pocket-reader-random-item (prefix)
  "Open a random item from the current list.
With universal prefix, read a key and call the command bound to
that keystroke on a random item."
  (interactive "p")
  (let ((fn (or (and (> prefix 1)
                     (alist-get (read-key "Key: ") pocket-reader-mode-map))
                #'pocket-reader-open-url)))
    (pocket-reader--with-pocket-reader-buffer
      (cl-loop do (progn
                    (goto-char (random (buffer-size)))
                    (beginning-of-line))
               while (not (pocket-reader--item-visible-p))
               finally do (funcall fn)))))

(defun pocket-reader--column-beginning (column)
  "Return the position of the beginning of the column named COLUMN, in the current line.

Return nil if not found."
  (save-excursion
    (beginning-of-line)
    (let ((prop 'tabulated-list-column-name)
          (end (line-end-position)))
      (while (and (< (point) end)
                  (not (equal (get-text-property (point) prop) column)))
        (goto-char (next-single-property-change (point) prop nil end)))
      (and (< (point) end) (point)))))

(defun pocket-reader-excerpt ()
  "Show excerpt for marked or current items."
  (interactive)
  (pocket-reader--at-marked-or-current-items
    (let ((excerpt (pocket-reader--get-property 'excerpt)))
      (unless (s-blank-str? excerpt)
        (let* ((start-col (save-excursion
                            (goto-char (pocket-reader--column-beginning "Title"))
                            (current-column)))
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
                                  for excerpt = (pocket-reader--get-property 'excerpt)
                                  when excerpt
                                  return excerpt
                                  do (forward-line 1)
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
  (forward-line 1))

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

(defun pocket-reader-tag-search (tag)
  "Search for items with TAG.
This is a plain, simple tag search, not intended to be used with
other special keywords."
  ;; MAYBE: Maybe add support for special keywords, but that might
  ;; make it more complicated to use than it is worth, because it
  ;; would mean making every plain word an implied tag keyword.
  (interactive (list (completing-read "Tag: " (cons "_untagged_" (pocket-reader--all-tags)))))
  (let ((query (concat ":t:" tag)))
    (pocket-reader-search query)))

(defun pocket-reader-add-tags (tags)
  "Add TAGS to current item."
  (interactive (list (completing-read "Tags: " (pocket-reader--all-tags))))
  (let* ((new-tags (s-split (rx (or space ",")) tags 'omit-nulls))
         (new-tags-string (s-join "," new-tags)))
    (when (and new-tags-string
               (apply #'pocket-lib--tags-action 'tags_add new-tags-string
                      (pocket-reader--marked-or-current-items)))
      ;; Tags added successfully
      (pocket-reader--at-marked-or-current-items
        (pocket-reader--add-tags new-tags)))))

(defun pocket-reader-remove-tags (tags)
  "Remove TAGS from current item."
  ;; FIXME: Get all tags with a function.
  (interactive (list (completing-read "Tags: " (let (tags)
                                                 (pocket-reader--at-marked-or-current-items
                                                   (setq tags (append (pocket-reader--get-property 'tags) tags)))
                                                 (-sort #'string< (-uniq tags))))))
  (let* ((tags (s-split (rx (or space ",")) tags 'omit-nulls))
         (remove-tags-string (s-join "," tags)))
    (when (and remove-tags-string
               (apply #'pocket-lib--tags-action 'tags_remove remove-tags-string
                      (pocket-reader--marked-or-current-items)))
      ;; Tags removed successfully
      (pocket-reader--at-marked-or-current-items
        (pocket-reader--remove-tags tags)))))

(defun pocket-reader-set-tags (tags)
  "Set TAGS of current item."
  (interactive (list (completing-read "Tags: " (pocket-reader--all-tags))))
  (pocket-reader--with-pocket-reader-buffer
    (let* ((tags (s-split (rx (or space ",")) tags 'omit-nulls))
           (tags-string (s-join "," tags)))
      (when (apply #'pocket-lib--tags-action 'tags_replace tags-string (pocket-reader--marked-or-current-items))
        ;; Tags replaced successfully
        (pocket-reader--at-marked-or-current-items
          (pocket-reader--set-tags tags))))))

;;;;;; URL-opening

(cl-defun pocket-reader-open-url (&optional &key fn)
  "Open URL of current item with default function."
  (interactive)
  (pocket-reader--at-marked-or-current-items
    (let* ((id (tabulated-list-get-id))
           (item (ht-get pocket-reader-items id))
           (url (pocket-reader--get-url item))
           (fn (or fn (pocket-reader--map-url-open-fn url))))
      (when (funcall fn url)
        ;; Item opened successfully
        (when pocket-reader-archive-on-open
          (pocket-reader--with-pocket-reader-buffer
            (pocket-reader--archive-items (pocket-reader--current-item))))))))

(defun pocket-reader-pop-to-url ()
  "Open URL of current item with default pop-to function."
  (interactive)
  (pocket-reader-open-url :fn pocket-reader-pop-to-url-default-function))

(defun pocket-reader-open-in-external-browser ()
  "Open marked or current items in external browser.
The `browse-url-default-browser' function is used."
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
  (when-let ((id (tabulated-list-get-id))
             (item (ht-get pocket-reader-items id))
             (url (pocket-reader--get-url item)))
    (kill-new url)
    (message url)))

;;;;;; Other

(defun pocket-reader-delete ()
  "Delete current or marked items (with confirmation)."
  (interactive)
  (when (yes-or-no-p "Delete item(s)?")
    (apply #'pocket-reader--delete-items (pocket-reader--marked-or-current-items))))

(defun pocket-reader-resort ()
  "Re-sort list."
  (interactive)
  (tabulated-list-sort 0)
  (tabulated-list-sort 0))

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

(defun pocket-reader--get-url (item &optional &key first)
  "Return URL for ITEM.
If FIRST is non-nil, return the first URL found, not the best
one.  ITEM should be a hash-table with the appropriate keys, one
of which is chosen as configured by
`pocket-reader-url-priorities'."
  (let ((prioritized-url (cl-loop for key in pocket-reader-url-priorities
                                  for url = (ht-get item key) ; Gets the URL
                                  when (s-present? url)
                                  return url)))
    (if first
        prioritized-url
      (if-let ((domain (pocket-reader--url-domain prioritized-url))
               (key (cl-loop for (key . vals) in pocket-reader-domain-url-type-map
                             when (member domain vals)
                             return key))
               (domain-preferred-url (ht-get item key)))
          ;; Return domain-specific URL type
          domain-preferred-url
        ;; Return standard, prioritized URL type
        (or prioritized-url
            (error "No URL for item: %s" item))))))

(defun pocket-reader--item-visible-p ()
  "Return non-nil if current item is visible (i.e. not hidden by an overlay)."
  (cl-loop for ov in (overlays-at (line-beginning-position))
           never (string= "" (ov-val ov 'display))))

(defun pocket-reader--add-items (items)
  "Add ITEMS to `pocket-reader-items' and update display."
  (--each items
    (let* ((item (ht<-alist (cdr it) #'eq))
           (id (string-to-number (ht-get item 'item_id)))
           (domain (pocket-reader--url-domain (pocket-reader--get-url item)))
           (tags (pocket-lib--process-tags (ht-get item 'tags))))
      (ht-set item 'domain domain)
      (ht-set item 'tags tags)
      (ht-set pocket-reader-items id item)))

  (pocket-reader--set-tabulated-list-format)

  ;; Use a copy of the list.  Otherwise, when the tabulated list is sorted, `pocket-reader-items'
  ;; gets rearranged when `tabulated-list-entries' gets sorted, and that somehow causes the apparent
  ;; length of `pocket-reader-items' to change, and that causes items to disappear from the list
  ;; when `pocket-reader-more' is called.  This is a very strange bug, but it's basically caused by
  ;; `sort' modifying lists by side effects.  Making `tabulated-list-entries' a copy avoids this
  ;; problem while allowing them to share the underlying items, which aren't changed.
  (setq tabulated-list-entries (pocket-reader--items-to-tabulated-list-entries pocket-reader-items))

  (tabulated-list-init-header)
  (tabulated-list-print 'remember-pos)
  (pocket-reader--finalize))

(defun pocket-reader--items-to-tabulated-list-entries (items)
  "Convert ITEMS to a list of vectors of lists.
Suitable for `tabulated-list-entries'."
  ;; NOTE: From Emacs docs:

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
  (cl-loop for it being the hash-values of items
           collect (let ((id (string-to-number (ht-get it 'item_id)))
                         (added (pocket-reader--format-timestamp (string-to-number (ht-get it 'time_added))))
                         (favorite (pocket-reader--favorite-string (ht-get it 'favorite)))
                         (title (pocket-reader--not-empty-string (pocket-reader--or-string-not-blank
                                                                  (ht-get it 'resolved_title)
                                                                  (ht-get it 'given_title)
                                                                  "[untitled]")))
                         (domain (pocket-reader--url-domain
                                  ;; Don't use --get-url here, because, e.g. we don't want an "amp." to be shown in the list
                                  (pocket-reader--or-string-not-blank (ht-get it 'resolved_url)
                                                                      (ht-get it 'given_url))))
                         (tags (pocket-reader--not-empty-string (s-join "," (ht-get it 'tags)))))
                     (list id (vector added favorite title domain tags)))))

(defun pocket-reader--delete-items (&rest items)
  "Delete ITEMS.
Items should be a list of items as returned by
`pocket-reader--marked-or-current-items'."
  (when (apply #'pocket-lib-delete items)
    (cl-loop for item in items
             for id = (alist-get 'item_id item)
             do (progn
                  (ht-remove! pocket-reader-items id)
                  (pocket-reader--unmark-item id)
                  (pocket-reader--at-item id
                    (tabulated-list-delete-entry))))
    ;; Do this once, at the end, not for each item

    ;; TODO: Is this even necessary?  If so, should we just use
    ;; `cl-delete' instead of rebuilding it from scratch?  Or is it
    ;; better, safer, to do this?
    (setq tabulated-list-entries (pocket-reader--items-to-tabulated-list-entries pocket-reader-items))))

(defun pocket-reader--finalize (&rest _)
  "Finalize the buffer after adding or sorting items."
  ;; Because we have to add this function as advice to
  ;; `tabulated-list--sort-by-column-name', causing it to run in every
  ;; tabulated-list buffer, we must make sure it's the pocket-reader
  ;; buffer.
  (when (string= "*pocket-reader*" (buffer-name))
    (run-hooks 'pocket-reader-finalize-hook)))

(defun pocket-reader--parse-query (query)
  "Return plist representing parsed QUERY string."
  (let (parsed)
    (with-temp-buffer
      (insert query)
      (goto-char (point-min))
      (with-peg-rules
          ((query (+ term))
           (term (and (opt (* [blank]))
                      (or favorite archive unread all count tag plain-term)))
           (favorite (or ":*" ":favorite")
                     `(_ -- (setf (plist-get parsed :favorite) t)))
           (archive ":archive"
                    `(_ -- (setf (plist-get parsed :archive) t)))
           (unread ":unread"
                   `(_ -- (setf (plist-get parsed :unread) t)))
           (all ":all"
                `(_ -- (setf (plist-get parsed :all) t)))
           (count ":" (substring (+ [0-9]))
                  `(num -- (setf (plist-get parsed :count) (string-to-number num))))
           (tag (and (or ":t:" "t:") (or quoted-tag unquoted-tag)))
           (quoted-tag (and "\"" (substring (+ word (opt (* [blank])))) "\"")
                       `(tag -- (setf (plist-get parsed :tag) tag)))
           (unquoted-tag (substring word)
                         `(tag -- (setf (plist-get parsed :tag) tag)))
           (word (+ (or "_" (syntax-class word))))
           (plain-term (substring word)
                       `(word -- (push word (plist-get parsed :words)))))
        (peg-run (peg query))))
    parsed))

(defun pocket-reader--get-items (&optional query)
  "Return Pocket items for QUERY.
QUERY is a string which may contain certain keywords:

:*, :favorite  Return only favorited items.
:archive       Return only archived items.
:unread        Return only unread items (default).
:all           Return all items.
:COUNT         Return at most COUNT (a number) items.
:t:TAG, t:TAG  Return items with TAG (only one tag may be searched for)."
  ;; NOTE: ht version
  (let* ((query (or query ""))
         (parsed (pocket-reader--parse-query query))
         (states (remq nil
                       (list (when (plist-get parsed :archive)
                               "archive")
                             (when (plist-get parsed :all)
                               "all")
                             (when (plist-get parsed :unread)
                               "unread"))))
         (state (progn
                  (when states
                    (unless (= 1 (length states))
                      (user-error "Only one of :archive, :all, or :unread may be used")))
                  (car states)))
         (favorite (when (plist-get parsed :favorite)
                     1))
         (count (setq pocket-reader-show-count (or (plist-get parsed :count) pocket-reader-show-count)))
         (tag (plist-get parsed :tag))
         (query-string (s-join " " (plist-get parsed :words)))
         (items (cdr (cl-third (pocket-lib-get :detail-type "complete" :count count :offset pocket-reader-offset
                                 :search query-string :state state :favorite favorite :tag tag)))))
    (when (> (length items) 0)
      ;; Empty results return an empty vector, which evaluates non-nil, which isn't useful, so in that case we return nil instead.
      items)))

(defun pocket-reader--action (action &rest _)
  "Execute ACTION on marked or current items.
ACTION should be a string or symbol which is the name of an
action in the Pocket API."
  ;; MAYBE: Not currently using this, may not need it.
  (pocket-reader--with-pocket-reader-buffer
    (apply #'pocket-lib--action action (pocket-reader--marked-or-current-items))))

(defun pocket-reader--marked-or-current-items ()
  "Return marked or current items, suitable for passing to `pocket-lib' functions."
  (or (cl-loop for (id . ov) in pocket-reader-mark-overlays
               collect (list (cons 'item_id id)))
      (list (pocket-reader--current-item))))

(defun pocket-reader--set-tabulated-list-format ()
  "Set `tabulated-list-format'.
Sets according to the maximum width of items about to be
displayed."
  (when-let ((domain-width (cl-loop for item being the hash-values of pocket-reader-items
                                    maximizing (length (ht-get item 'domain))))
             (title-width (- (window-text-width) 11 2 domain-width 10 1)))
    (when (> domain-width pocket-reader-site-column-max-width)
      (setq domain-width pocket-reader-site-column-max-width))
    (setq tabulated-list-format (vector (list "Added" 10 pocket-reader-added-column-sort-function)
                                        (list "*" 1 t)
                                        (list "Title" title-width t)
                                        (list "Site" domain-width t)
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
  "Return list containing cons of current item's ID.
Suitable for passing to pocket-lib."
  (let* ((id (tabulated-list-get-id)))
    (list (cons 'item_id id))))

(defun pocket-reader--get-property (property)
  "Return value of PROPERTY for current item."
  (let ((id (tabulated-list-get-id)))
    (ht-get* pocket-reader-items id property)))

(defun pocket-reader--set-property (property value)
  "Set current item's PROPERTY to VALUE."
  (pocket-reader--with-pocket-reader-buffer
    (let* ((id (tabulated-list-get-id))
           (item (ht-get pocket-reader-items id)))
      (ht-set! item property value))))

(defun pocket-reader--url-domain (url)
  "Return domain for URL.
Common prefixes like www are removed."
  (replace-regexp-in-string (rx bos (and (or "www" "amp") ".")) ""
                            (url-host (url-generic-parse-url url))))

(defun pocket-reader--format-timestamp (timestamp)
  "Format TIMESTAMP."
  (format-time-string "%Y-%m-%d" timestamp))

(cl-defun pocket-reader--add-spacers (&key (min-group-size 2))
  "Insert overlay spacers where the current sort column's values change.
For example, if sorted by date, a spacer will be inserted where
the date changes.  If no group has at least MIN-GROUP-SIZE items,
no spacers will be inserted. "
  ;; TODO: Use column-specific functions so that, e.g. date column could be grouped by month/year
  (let ((sort-column (seq-position tabulated-list-format tabulated-list-sort-key
                                   (lambda (seq elt)
                                     (string= (car seq) (car elt))))))
    ;; Clear existing spacers
    (ov-clear)
    (save-excursion
      (goto-char (point-min))
      (cl-loop with largest-group-size = 1
               with prev-data = (elt (tabulated-list-get-entry) sort-column)
               while (not (eobp))
               do (forward-line 1)
               for current-data = (elt (tabulated-list-get-entry) sort-column)
               if (not (equal current-data prev-data))
               do (progn
                    (ov (line-beginning-position) (line-beginning-position) 'before-string "\n")
                    (setq prev-data current-data))
               else do (cl-incf largest-group-size)
               finally do (when (< largest-group-size min-group-size)
                            (ov-clear))))))

;;;;;; Archived/readd

(defun pocket-reader--archive-items (&rest items)
  "Mark ITEMS as archived."
  (when (apply #'pocket-lib-archive items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property 'status "1")
             (pocket-reader--apply-faces-to-line))
           items)))

(defun pocket-reader--readd-items (&rest items)
  "Readd ITEMS."
  (when (apply #'pocket-lib-readd items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property 'status "0")
             (pocket-reader--apply-faces-to-line))
           items)))

(defun pocket-reader--is-archived ()
  "Return non-nil if current item is archived."
  (string= "1" (pocket-reader--get-property 'status)))

;;;;;; Favorites

(defun pocket-reader--favorite-items (&rest items)
  "Mark ITEMS as favorites."
  (when (apply #'pocket-lib-favorite items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property 'favorite "1")
             (pocket-reader--update-favorite-display t))
           items)))

(defun pocket-reader--unfavorite-items (&rest items)
  "Unmark ITEMS as favorites."
  (when (apply #'pocket-lib-unfavorite items)
    (--map (pocket-reader--at-item it
             (pocket-reader--set-property 'favorite "0")
             (pocket-reader--update-favorite-display nil))
           items)))

(defun pocket-reader--is-favorite ()
  "Return non-nil if current item is a favorite."
  (string= "1" (pocket-reader--get-property 'favorite)))

(defun pocket-reader--update-favorite-display (is-favorite)
  "Update favorite star for current item, depending on value of IS-FAVORITE."
  (tabulated-list-set-col 1 (if is-favorite "*" "") t)
  (pocket-reader--apply-faces-to-line))

;;;;;; Tags

(defun pocket-reader--all-tags ()
  "Return list of all tags in the current list."
  (pocket-reader--with-pocket-reader-buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (not (eobp))
               for tags = (pocket-reader--get-property 'tags)
               when tags
               append tags into list
               do (forward-line 1)
               finally return (-sort #'string< (-uniq list))))))

(defun pocket-reader--add-tags (tags)
  "Add TAGS to current item.
TAGS should be a list of strings."
  (let* ((old-tags (pocket-reader--get-property 'tags))
         (new-tags (append old-tags tags)))
    (pocket-reader--set-tags new-tags)))

(defun pocket-reader--remove-tags (tags)
  "Remove TAGS from current item.
TAGS should be a list of strings."
  (let* ((old-tags (pocket-reader--get-property 'tags))
         (new-tags (seq-difference old-tags tags #'string=)))
    (pocket-reader--set-tags new-tags)))

(defun pocket-reader--set-tags (tags)
  "Set current item's tags to TAGS.
TAGS should be a list of strings.  Tags are sorted and
deduplicated."
  (let* ((tags (-sort #'string< (-uniq tags))))
    (pocket-reader--set-property 'tags tags)
    (pocket-reader--set-tags-column)
    (pocket-reader--apply-faces-to-line)))

(defun pocket-reader--set-tags-column ()
  "Set tags column for current entry.
Gets tags from text property."
  (tabulated-list-set-col 4 (s-join "," (pocket-reader--get-property 'tags)) 'change-entry-data))

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
  (setq pocket-reader-mark-overlays (cl-remove id pocket-reader-mark-overlays :test #'= :key #'car)))

(defun pocket-reader--item-marked-p ()
  "Return non-nil if current item is marked."
  (let ((id (tabulated-list-get-id)))
    (cl-member id pocket-reader-mark-overlays
               :test #'= :key #'car)))

;;;;;; Sorting

(defun pocket-reader--added-fancy< (a b)
  "Return non-nil if A should be sorted before B.
Items are compared by date, then favorite status, then tags, then
domain.  Suitable for sorting `tabulated-list-entries'."
  (cl-flet ((day (it) (let* ((id (car it))
                             (added-string (ht-get* pocket-reader-items id 'time_added)) )
                        (time-to-days (string-to-number added-string)))))
    (let* ((a-day (day a))
           (b-day (day b)))
      (if (= a-day b-day)
          ;; Same day: compare favorite, then tags, then domain
          (cl-case (pocket-reader--compare-favorite a b)
            ('< nil)
            ('> t)
            ('= (cl-case (pocket-reader--compare-tags a b)
                  ('< nil)
                  ('> t)
                  ('=
                   ;; Same tags; compare domain (invert since the default order is descending)
                   (not (pocket-reader--domain< a b))))))
        ;; Different day: compare day
        (< a-day b-day)))))

(defun pocket-reader--added< (a b)
  "Return non-nil if A's `time_added' timestamp is before B's.
Suitable for sorting `tabulated-list-entries'."
  (cl-flet ((added (it) (let ((id (car it)))
                          (string-to-number (ht-get* pocket-reader-items id 'time_added)))))
    (let ((a-added (added a))
          (b-added (added b)))
      (< a-added b-added))))

(defun pocket-reader--domain< (a b)
  "Return non-nil if A's domain is alphabetically before B's."
  (cl-flet ((domain (it) (let ((id (car it)))
                           (pocket-reader--url-domain (pocket-reader--get-url (ht-get pocket-reader-items id)
                                                                              :first t)))))
    (string< (domain a) (domain b))))

(defun pocket-reader--compare-favorite (a b)
  "Compare A's and B's favorite statuses.
If both are the same, return `='.  If only A is a favorite,
return `>'.  If only B, return `<'."
  (cl-flet ((fav (it) (let ((id (car it)))
                        (pcase (ht-get* pocket-reader-items id 'favorite)
                          ("0" nil)
                          ("1" t)))))
    (let ((a-fav (fav a))
          (b-fav (fav b)))
      (cond ((eq a-fav b-fav) '=)
            (a-fav '<)
            (b-fav '>)))))

(defun pocket-reader--compare-tags (a b)
  "Compare A's and B's lists of tags.
If they are the same, return `='.  If they have different numbers
of tags, return `<' if A has more, or `>' if B has more.  If they
have the same number of tags, join each list into a single string
and compare them with `string='."
  (cl-flet ((tags (it) (let ((id (car it)))
                         (ht-get* pocket-reader-items id 'tags))))
    (let ((a-tags (tags a))
          (b-tags (tags b)))
      (if (not (or a-tags b-tags))
          ;; No tags
          '=
        ;; Some tags
        (if (not (and a-tags b-tags))
            ;; One item has no tags
            (if a-tags
                '<
              '>)
          ;; Both items have tags
          (let ((a-length (length a-tags))
                (b-length (length b-tags)))
            (if (/= a-length b-length)
                ;; Different number of tags: sort by number of tags
                (if (< a-length b-length)
                    ;; Entries with more tags should be sorted earlier
                    '>
                  '<)
              ;; Same number of tags: sort string of tags alphabetically
              (let ((a-string (s-join "" a-tags))
                    (b-string (s-join "" b-tags)))
                (cond ((string= a-string b-string) '=)
                      ((string< a-string b-string) '<)
                      (t '>))))))))))

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
  "Apply faces to buffer."
  ;; TODO: Maybe we should use a custom print function but this is simpler
  (pocket-reader--with-pocket-reader-buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (pocket-reader--apply-faces-to-line)
        (forward-line 1))
      (goto-char (point-min)))))

(defun pocket-reader--apply-faces-to-line ()
  "Apply faces to current line."
  (pocket-reader--with-pocket-reader-buffer
    (add-text-properties (line-beginning-position) (line-end-position)
                         (list 'face (pcase (pocket-reader--get-property 'status)
                                       ("0" 'pocket-reader-unread)
                                       ("1" 'pocket-reader-archived)) ))
    (when (pocket-reader--get-property 'favorite)
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
  (pocket-reader--with-pocket-reader-buffer
    (-when-let* ((start (pocket-reader--column-beginning column))
                 (end (next-single-char-property-change start
                                                        'tabulated-list-column-name
                                                        nil
                                                        (line-end-position))))
      (add-face-text-property start end face t))))

;;;;; URL-adding helpers

;;;###autoload
(defun pocket-reader-add-link ()
  "Add link at point to Pocket.
This function tries to work in multiple major modes, such as w3m,
eww, elfeed, and Org."
  (interactive)
  (cl-case major-mode
    ('eww-mode (pocket-reader-eww-add-link))
    ('org-mode (pocket-reader-org-add-link))
    ('w3m-mode (pocket-reader-w3m-add-link))
    ('shr-mode (pocket-reader-shr-add-link))
    ('elfeed-search-mode (pocket-reader-elfeed-search-add-link))
    ('elfeed-show-mode (pocket-reader-elfeed-entry-add-link))
    (t (pocket-reader-generic-add-link))))

;;;###autoload
(defun pocket-reader-eww-add-link ()
  "Add link at point to Pocket in eww buffers."
  (interactive)
  ;; `eww-links-at-point' returns a list of links, but we only use the
  ;; first one.  I think this is the right thing to do in most, if not
  ;; all, cases.
  (when-let ((url (car (eww-links-at-point))))
    (when (pocket-lib-add-urls url)
      (message "Added: %s" url))))

;;;###autoload
(defun pocket-reader-org-add-link ()
  "Add link at point to Pocket in Org buffers."
  (interactive)
  (when-let ((url (when (org-in-regexp org-bracket-link-regexp 1)
                    (org-link-unescape (match-string-no-properties 1)))))
    (when (pocket-lib-add-urls url)
      (message "Added: %s" url))))

(declare-function 'w3m-with-lnum 'w3m-lnum)
(declare-function 'w3m-lnum-read-interactive 'w3m-lnum)
(declare-function 'w3m-lnum-get-anchor-info 'w3m-lnum)
(defvar last-index)
(defvar w3m-current-url)
;;;###autoload
(with-eval-after-load 'w3m-lnum
  (cl-defun pocket-reader-w3m-lnum-add-link (&key (type 1))
    "Add link to Pocket with lnum in w3m buffers."
    (interactive)
    (w3m-with-lnum
     type ""
     (when-let ((num (car (w3m-lnum-read-interactive
                           "Anchor number: "
                           'w3m-lnum-highlight-anchor
                           type last-index w3m-current-url)))
                (info (w3m-lnum-get-anchor-info num))
                (url (car info)))
       (when (pocket-lib-add-urls url)
         (message "Added: %s" url))))))

;;;###autoload
(with-eval-after-load 'w3m
  (defun pocket-reader-w3m-add-link ()
    "Add link at point to Pocket in w3m buffers."
    (interactive)
    (if-let ((url (or (get-text-property (point) 'w3m-href-anchor)
                      (unless (bolp)
                        (save-excursion
                          (get-text-property (1- (point)) 'w3m-href-anchor)))
                      (unless (eolp)
                        (save-excursion
                          (get-text-property (1+ (point)) 'w3m-href-anchor)))
                      (thing-at-point-url-at-point))))
        (when (pocket-lib-add-urls url)
          (message "Added: %s" url))
      (if (member 'w3m-lnum-mode minor-mode-list)
          ;; No URL found around point: use lnum if loaded
          (pocket-reader-w3m-lnum-add-link)
        ;; We tried.
        (message "No URL found around point.")))))

;;;###autoload
(defun pocket-reader-shr-add-link ()
  "Add link at point in `shr-mode' buffer to Pocket."
  (interactive)
  (if-let ((url (get-text-property (point) 'shr-url)))
      (when (pocket-lib-add-urls url)
        (message "Added: %s" url))
    (message "No URL found at point.")))

(defvar elfeed-show-entry)
;;;###autoload
(with-eval-after-load 'elfeed
  (defun pocket-reader-elfeed-search-add-link ()
    "Add links for selected entries in Elfeed search-mode buffer to Pocket.
This is only for the elfeed-search buffer, not for entry buffers."
    (interactive)
    (when-let ((entries (elfeed-search-selected))
               (links (mapcar #'elfeed-entry-link entries)))
      (when (pocket-lib-add-urls links)
        (message "Added: %s" (s-join ", " links))
          (elfeed-search-untag-all-unread))))

  (defun pocket-reader-elfeed-entry-add-link ()
    "Add links for selected entries in elfeed-show-mode buffer to Pocket.
This is only for the elfeed-entry buffer, not for search buffers."
    (interactive)
    (when-let ((link (elfeed-entry-link elfeed-show-entry)))
      (when (pocket-lib-add-urls link)
        (message "Added: %s" link)))))

;;;###autoload
(defun pocket-reader-generic-add-link ()
  "Try to add URL at point to Pocket using `thing-at-pt'."
  (interactive)
  (if-let ((url (or (thing-at-point-url-at-point)
                    (with-temp-buffer
                      (insert (current-kill 0))
                      (thing-at-point-url-at-point)))))
      (when (pocket-lib-add-urls url)
        (message "Added: %s" url))
    (user-error "No URL found at point or in clipboard")))


;;;; Footer

(provide 'pocket-reader)

;;; pocket-reader.el ends here
