#|

This file is a part of DATALOADER project.
Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

DATALOADER is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

DATALOADER is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
DATALOADER.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(uiop:define-package dataloader.mime
  (:use))
(uiop:define-package dataloader
  (:use)
  (:mix :numcl :iterate :trivia :alexandria)
  (:local-nicknames (:mime :dataloader.mime))
  (:shadow :load)
  (:export #:load
           #:save))
(in-package :dataloader)

;; blah blah blah.


