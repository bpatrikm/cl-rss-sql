;;cl-rss-sql, a utility to track and view (some) RSS published content
;;Copyright (C) 2019 Patrik Magnusson

;;This program is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.(in-package :cl-gtk-frank.builder)

;;sbcl --load "lisp/cl-rss-sql/compile.lisp"
(require :cl-rss-sql)
(require :clsql-mysql);not just :clsql, since the results in warning messages on each execution, which is an issue if run in a cronjob. Replace by desired backend, if other than mysql.
(sb-ext:save-lisp-and-die "cl-rss-sql" :executable t :compression 9)
