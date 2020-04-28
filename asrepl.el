;;; asrepl.el --- A socket REPL -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200428
;; Package-Requires: ((smartparens "1.11.0") (emacs "26.2"))
;; Keywords: socket repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A socket REPL - simple socket repl interaction and editor functionality

;;;; Installation

;;;;; Manual

;; Ensure this file and the following dependencies (and their
;; dependencies) are in your load-path:
;;
;;   smartparens
;;
;;  and put this in your relevant init file:
;;
;;    (require 'asrepl)
;;
;;  Optionally, add:
;;
;;    (add-hook '<something>-mode-hook
;;              #'asrepl-interaction-mode)
;;
;;  for editor features to be enabled when visiting a buffer with
;;  <something> code in it (e.g. <something> might be clojure or janet)

;;;;; Automatic

;; TODO :)

;;;;; Usage

;; 0. Start up a Lisp project (Babashka, JVM Clojure, shadow-cljs, Arcadia,
;;    Clojure CLR, Janet) with a socket repl and note the host and port

;; 1. Connect to the socket repl by:
;;
;;      M-x asrepl
;;
;;    and at the prompt, specify a host and port like:
;;
;;      localhost:23579
;;
;;    i.e. a host or ip address followed by a colon and port number
;;
;;    A buffer for interaction with the socket repl should appear.

;; 2. For editor features, in a relevant buffer with a Lisp source file:
;;
;;      M-x asrepl-interaction-mode
;;
;;    There should be a Asrepl menu containing some convenience commands:
;;
;;      Send buffer
;;      Send expression at point
;;      Send region
;;
;;      Switch to REPL

;;;;; Acknowledgments

;; Thanks to those involved in:
;;
;;   clojure
;;   emacs
;;   janet
;;   smartparens
;;
;; and transitively involved folks too ;)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'comint)
(require 'smartparens)
(require 'subr-x)

;;;; The Rest

(defgroup asrepl nil
  "A socket REPL"
  :prefix "asrepl-"
  :group 'applications)

(defcustom asrepl-default-endpoint "localhost:23579"
  "Default host and port to connect to.

Host and port should be delimited with ':'."
  :type 'string
  :group 'asrepl)

(defvar asrepl-repl-buffer-name "*asrepl-repl*"
  "Name of repl buffer.")

(defun asrepl-switch-to-repl ()
  "Switch to the repl buffer named by `asrepl-repl-buffer-name`."
  (interactive)
  (pop-to-buffer asrepl-repl-buffer-name))

(defun asrepl-send-code (code-str)
  "Send CODE-STR.

CODE-STR should be a lisp form."
  (interactive "sCode: ")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer asrepl-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." asrepl-repl-buffer-name))
      ;; switch to asrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (insert code-str)
      (comint-send-input)
      (set-buffer original-buffer)
      (if (eq original-buffer repl-buffer)
          (goto-char (point-max))
        (goto-char here)))))

(defun asrepl-send-region (start end &optional pre post)
  "Send a region bounded by START and END.

Optional arguments PRE and POST are strings to insert before
and after the region about to be sent, respectively."
  (interactive "r")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer asrepl-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." asrepl-repl-buffer-name))
      ;; switch to asrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (when pre
        (insert pre))
      ;; switch back
      (set-buffer original-buffer)
      (append-to-buffer repl-buffer start end)
      (set-buffer repl-buffer)
      (when post
        (insert post))
      (comint-send-input)
      (set-buffer original-buffer)
      (goto-char here))))

(defun asrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (asrepl-send-region (point-min) (point-max)))

;; XXX: figure out how to do this without smartparens
(defun asrepl-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (let* ((thing (sp-get-thing t))
         (start (sp-get thing :beg))
         (end (sp-get thing :end)))
    (when (and start end)
      (asrepl-send-region start end))))

(defvar asrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'asrepl-send-buffer)
    (define-key map "\C-c\C-e" 'asrepl-send-expression-at-point)
    (define-key map "\C-c\C-r" 'asrepl-send-region)
    (define-key map "\C-c\C-z" 'asrepl-switch-to-repl)
    (easy-menu-define asrepl-interaction-mode-map map
      "A socket REPL Interaction Mode Menu"
      '("Asrepl"
        ["Send buffer" asrepl-send-buffer t]
        ["Send expression at point" asrepl-send-expression-at-point t]
        ["Send region" asrepl-send-region t]
        "--"
        ["Switch to REPL" asrepl-switch-to-repl t]))
    map)
  "Asrepl interaction mode map.")

(defvar asrepl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
        (easy-menu-define asrepl-mode-map map
          "A socket REPL Mode Menu"
          '("Asrepl"
            ["Switch to other window" other-window t]))
    map)
  "Asrepl mode map.")

(define-derived-mode asrepl-mode comint-mode "A socket REPL"
  "Major mode for asrepl.

\\{asrepl-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s")))

;;;###autoload
(define-minor-mode asrepl-interaction-mode
  "Minor mode for asrepl interaction from a lisp buffer.

The following keys are available in `asrepl-interaction-mode`:

\\{asrepl-interaction-mode}"

  nil " asrepl" asrepl-interaction-mode-map)

;;;###autoload
(defun asrepl (endpoint)
  "Start asrepl.

Query user for ENDPOINT which specifies the socket REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (let ((endpoint asrepl-default-endpoint))
     (list
      (read-string (format "REPL endpoint (default '%s'): " endpoint)
                   endpoint nil endpoint))))
  (unless
      ;(ignore-errors ;; XXX: uncomment at some point...
        (let* ((ep (split-string endpoint ":"))
               (host (car ep))
               (port (string-to-number (cadr ep))))
          (message "Connecting to socket REPL on '%s:%d'..." host port)
          (with-current-buffer (get-buffer-create asrepl-repl-buffer-name)
            (prog1
                (make-comint-in-buffer "asrepl" asrepl-repl-buffer-name
                                       (cons host port))
              (goto-char (point-max))
              (asrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max)))))
    (message "Failed to connect to %s" endpoint)))

(provide 'asrepl)

;;; asrepl.el ends here
