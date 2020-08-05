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

(provide 'asrepl-clojure)

;;; asrepl-clojure.el ends here
