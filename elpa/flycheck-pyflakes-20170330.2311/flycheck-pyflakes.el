;;; flycheck-pyflakes.el --- Support pyflakes in flycheck

;; Copyright (C) 2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 29 April 2014
;; Version: 0.1
;; Package-Version: 20170330.2311
;; Package-Commit: 61b045939e3743b2162b7e4e73249c66fc2b8f65
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:

;; This package adds support for pyflakes to flycheck. To use it, add
;; to your init.el:

;; (require 'flycheck-pyflakes)
;; (add-hook 'python-mode-hook 'flycheck-mode)

;; If you want to use pyflakes you probably don't want pylint or
;; flake8. To disable those checkers, add the following to your
;; init.el:

;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

(flycheck-define-checker python-pyflakes
  "A Python syntax and style checker using the pyflakes utility.

To override the path to the pyflakes executable, set
`flycheck-python-pyflakes-executable'.

See URL `http://pypi.python.org/pypi/pyflakes'."
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-pyflakes)

(provide 'flycheck-pyflakes)
;;; flycheck-pyflakes.el ends here
