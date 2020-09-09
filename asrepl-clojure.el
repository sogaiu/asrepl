;;; asrepl-clojure.el --- Clojure bits for asrepl -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200730
;; Package-Requires: ((smartparens "1.11.0") (emacs "26.2"))
;; Keywords: socket repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Clojure-specific bits for asrepl

;;;; Installation

;;;;; Manual

;; Ensure this file and the following dependencies (and their
;; dependencies) are in your load-path:
;;
;;   asrepl
;;
;;  and put this in your relevant init file:
;;
;;    (require 'asrepl-clojure)
;;
;; XXX: hook instructions?

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

(require 'asrepl)

;;;; The Rest

(defun asrepl-clojure-load-file (filename)
  "Send the `load-file` form with full path of FILENAME."
  (interactive "fFile name: ")
  (asrepl-send-code (format "(load-file \"%s\")"
                            (expand-file-name filename))))

(defun asrepl-clojure-load-buffer-file ()
  "Send the `load-file` form with buffer's full path."
  (interactive)
  (asrepl-clojure-load-file (buffer-file-name)))

(defun asrepl-clojure-load-buffer-file-and-switch-to-ns ()
  "Send the `load-file` form with buffer's full path."
  (interactive)
  (when (executable-find "alc.detect-ns")
    (let ((file-path (buffer-file-name)))
      (condition-case nil
          (let ((lines (process-lines "alc.detect-ns" file-path)))
            ;; XXX: check for sane values?
            (if (not (string-equal (car lines) ""))
              (asrepl-send-code (format "(do (load-file \"%s\") (in-ns '%s))"
                                        file-path
                                        (first lines)))
              ;; XXX: throw error instead?
              (error "Failed to detect ns for: %s" file-path)))
        (error (message "Failed to detect ns for: %s" file-path))))))

(defun asrepl-load-minibuffer-history-with-endpoints ()
  "Update `minibuffer-history` using enum-repls, if available."
  (when (executable-find "enum-repls")
    (when-let ((project-dir (cdr (project-current))))
      (let ((ports
             (thread-last
                 (process-lines "enum-repls"
                                (file-truename project-dir))
              (seq-map (lambda (line)
                         (nth 1 (split-string line)))))))
        (seq-do (lambda (elt)
                  (add-to-history #'minibuffer-history
                                  (format "localhost:%s" elt)))
                ports)))))

;;;###autoload
(defun asrepl-clojure (endpoint)
  "Start asrepl.

Query user for ENDPOINT which specifies the socket REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (if (not (buffer-file-name)) ; XXX: loose
       (user-error "Please invoke when visiting a Clojure file")
     (let ((endpoint
            (or
             (if-let ((a-port
                       (nth 0
                            (asrepl-load-minibuffer-history-with-endpoints))))
                 (format "localhost:%s" a-port)
               asrepl-default-endpoint))))
       (list
        (read-string (format "REPL endpoint (default '%s'): " endpoint)
                     endpoint nil endpoint)))))
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

(provide 'asrepl-clojure)

;;; asrepl-clojure.el ends here
