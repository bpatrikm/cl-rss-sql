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

(in-package :cl-rss-sql)

;;sätt character_set_{results/client/connection} till utf8 i mysql-server innan clsql laddas

(file-enable-sql-reader-syntax)
(setf clsql:*default-caching* nil)

(defclass rss-publication ()
  ((idpublication :initarg :idpublication :reader idpublication)
   (published :initarg :published :reader published)
   (latest-revision :initform nil :initarg :latest-revision :reader latest-revision)
   (title :initarg :title :reader title)
   (desc :initarg :desc :reader desc)
   ;;(thumb :initarg :thumb :reader thumb)
   (url :initarg :url :reader url :initform nil);används inte av youtube/bitchute
   ))

;;a är den som eventuellt är nyare
(defmethod pub-diff ((a rss-publication) (b rss-publication))
  (unless (equal (idpublication a) (idpublication b))
    (error "pub-diff, not same id"))
  (let ((title (unless (equal (title a) (title b))
                 (title a)))
        (desc (unless (equal (desc a) (desc b))
                (desc a)))
        ;;thumb
        )
    (when (or title desc)
      (make-instance 'rss-publication
                     :idpublication (idpublication a)
                     :published (published a)
                     :title title
                     :desc desc))))

(defun extract-single-by-name (treenode name)
  (xmlrep-string-child
   (car (xmlrep-find-child-tags name treenode))
   ""))
     
    
(defun compile-rss-youtube (treenode)
  (let ((entries
         (xmlrep-find-child-tags "entry" treenode)))
    (mapcar (lambda (entry)
              (let ((id (extract-single-by-name entry "videoId"))
                    (title (extract-single-by-name entry "title"))
                    (published (extract-single-by-name entry "published"))
                    (media-group (xmlrep-find-child-tag "group" entry)))
                (let ((desc (extract-single-by-name media-group "description"))
                      (thumb-url (xmlrep-attrib-value
                                  "url"
                                  (xmlrep-find-child-tag "thumbnail" media-group))))
                  (declare (ignore thumb-url))
                  (make-instance 'rss-publication
                                 :idpublication id
                                 :published published
                                 :title title
                                 :desc desc))))
            entries)))

(defun compile-rss-bitchute (treenode)
  (let ((items
         (let ((channel
                (xmlrep-find-child-tag "channel" treenode)))
           (xmlrep-find-child-tags "item" channel))))
    (mapcar (lambda (item)
              (let ((id (extract-single-by-name item "guid"))
                    (title (extract-single-by-name item "title"))
                    (published (with-database (db *db-cred* :database-type :mysql :pool t)
                                 (caar (query (sql-operation
                                               'function "select"
                                               (sql-operation
                                                'function "str_to_date"
                                                (extract-single-by-name item "pubDate")
                                                "%a, %d %b %Y %T")) :database db))))
                    (desc (extract-single-by-name item "description"))
                    (thumb-url
                     (xmlrep-attrib-value
                      "url"
                      (xmlrep-find-child-tag "enclosure" item))))
                (declare (ignore thumb-url))
                (make-instance 'rss-publication
                               :idpublication id
                               :published published
                               :title title
                               :desc desc)))
            items)))

(defun compile-rss-podcasts (treenode)
  (let ((items
         (let ((channel
                (xmlrep-find-child-tag "channel" treenode)))
           (xmlrep-find-child-tags "item" channel))))
    (mapcar (lambda (item)
              (let ((id (extract-single-by-name item "guid"))
                    (title (extract-single-by-name item "title"))
                    (published (with-database (db *db-cred* :database-type :mysql :pool t)
                                 (caar (query (sql-operation
                                               'function "select"
                                               (sql-operation
                                                'function "str_to_date"
                                                (extract-single-by-name item "pubDate")
                                                "%a, %d %b %Y %T")) :database db))))
                    (desc (extract-single-by-name item "description"))
                    (url
                     (xmlrep-attrib-value
                      "url"
                      (xmlrep-find-child-tag "enclosure" item))))
                (make-instance 'rss-publication
                               :idpublication id
                               :published published
                               :title title
                               :desc desc
                               :url url)))
            items)))

(defun compile-rss-file (filepath site)
  (with-open-file (in filepath)
    (let ((treenode (parse in)))
      (case site
        (:youtube (compile-rss-youtube treenode))
        (:bitchute (compile-rss-bitchute treenode))
        (:podcasts (compile-rss-podcasts treenode))
        (t (error "Unknown site"))))))

(def-view-class channel ()
  ((idchannel :column "idchannel" :db-kind :key :db-constraints :primary-key :initarg :idchannel :reader idchannel :db-type 'integer)
   (name :column "name" :initarg :name :reader name)
   (site-channel-id :column "site_channel_id" :initarg :site-channel-id :reader site-channel-id)
   (rss-url :column "rss_url" :reader rss-url);används inte för youtube/bitchute
   (site :column "site" :initarg :site :reader site));:youtube, :bitchute, :podcasts
  (:base-table "channel"))

(def-view-class publication ()
  ((idpublication :column "idpublication" :db-kind :key :db-constraints :primary-key :initarg :idpublication :reader idpublication)
   (published :column "published" :initarg :published :reader published)
   (idchannel :column "idchannel" :initarg :idchannel :reader idchannel :db-type 'integer);min interna id
   (first-view :column "first_view" :initform nil :initarg :first-view :reader first-view)
   (latest-view :column "latest_view" :initform nil :initarg :latest-view :reader latest-view)
   (fresh-p :column "fresh" :initform 1 :reader fresh-p)
   (url :column "url" :initform nil :initarg :url :reader url))
  (:base-table "publication"))

(def-view-class edition ()
  ((idpublication :column "idpublication" :db-kind :key :db-constraints :primary-key :initarg :idpublication :reader idpublication)
   (revision :column "revision" :db-kind :key :db-constraints :primary-key :initarg :revision :reader revision :db-type 'smallint)
   (received :column "received" :initarg :received :reader received)
   (title :column "title" :initarg :title :reader title :db-type 'string)
   (desc :column "description" :initarg :desc :reader desc :db-type 'string)
   (thumb :column "thumbnail" :initform nil :initarg :thumb :reader thumb))
  (:base-table "edition"))

;;jämför rss-uppslag med senaste db-edition, lägg in i publication/edition

;;title/desc/thumb är bara non-nil ifall de skiljer sig i den upplagan. Kombinera rader för att få fram 'senaste db-edition'

(defun latest-edition (idpublication)
  (with-database (db *db-cred* :database-type :mysql :pool t)
    (let ((publication (caar (select 'publication :where [= [idpublication] idpublication] :database db))))
      (when publication
        (let ((editions (sort (select 'edition :where [= [idpublication] idpublication] :database db :flatp t)
                              #'> :key #'revision)))
          (let ((latest-revision (revision (car editions)))
                (title (car (remove nil (mapcar #'title editions))))
                (desc (car (remove nil (mapcar #'desc editions))))
                ;;(thumb (car (remove nil (mapcar #'thumb editions))))
                )
            (make-instance 'rss-publication
                           :idpublication idpublication
                           :published (published publication)
                           :url (url publication)
                           :latest-revision latest-revision
                           :title title
                           :desc desc)))))))

(defun db-insert-if-new (rss-publication db-now idchannel)
  (let ((latest-edition (latest-edition (idpublication rss-publication))))
    (if (not latest-edition)
        (let ((new-publication (make-instance 'publication
                                              :idpublication (idpublication rss-publication)
                                              :published (published rss-publication)
                                              :url (url rss-publication)
                                              :idchannel idchannel))
              (new-edition (make-instance 'edition
                                          :idpublication (idpublication rss-publication)
                                          :revision 0
                                          :received db-now
                                          :title (title rss-publication)
                                          :desc (desc rss-publication))))
          (with-database (db *db-cred* :database-type :mysql :pool t)
            (update-records-from-instance new-publication :database db)
            (update-records-from-instance new-edition :database db)))
        (let ((diff (pub-diff rss-publication latest-edition)))
          (when diff
            (let ((new-edition (make-instance 'edition
                                              :idpublication (idpublication rss-publication)
                                              :revision (1+ (latest-revision latest-edition))
                                              :received db-now
                                              :title (title diff)
                                              :desc (desc diff))))
              (with-database (db *db-cred* :database-type :mysql :pool t)
                (update-records-from-instance new-edition :database db))))))))

(defun refresh-channel (channel)
  ;;töm katalogen som wget laddar ner till (~/.cl-rss-sql/)
  ;;tillverka wget-kommando -q (quiet) -T (timeout seconds) -Q (quota, t ex -Q2m för 2 MB, maxstorlek på nedladdning) -O (målfil)
  ;;kör wget
  ;;parsa fil, och db-insert-if-new
  ;;uppdatera latest-refresh
  (when (probe-file (pathname ".cl-rss-sql/rss.xml"))
    (delete-file (pathname ".cl-rss-sql/rss.xml")))
  (let ((db-now (with-database (db *db-cred* :database-type :mysql :pool t)
                  (caar (query "select now()" :database db))))
        (site (cond ((equal (site channel) "youtube") :youtube)
                    ((equal (site channel) "bitchute") :bitchute)
                    ((equal (site channel) "podcasts") :podcasts)
                    (t (error "Unknown site to refresh")))))
    (let ((rss-url (case site
                     (:youtube (format nil "https://www.youtube.com/feeds/videos.xml?channel_id=~a" (site-channel-id channel)))
                     (:bitchute (format nil "https://www.bitchute.com/feeds/rss/channel/~a/" (site-channel-id channel)))
                     (:podcasts (rss-url channel)))))
      (sb-ext:run-program "/usr/bin/wget" (list "-q" "-T15" "-Q2m" "-O" (namestring (merge-pathnames (pathname ".cl-rss-sql/rss.xml") (user-homedir-pathname))) rss-url) :output nil :input nil :wait t :error t)
      (when (probe-file (pathname ".cl-rss-sql/rss.xml"))
        (mapcar (lambda (rss-publication)
                  (db-insert-if-new rss-publication
                                    db-now
                                    (idchannel channel)))
                (compile-rss-file ".cl-rss-sql/rss.xml" site))
        (with-database (db *db-cred* :database-type :mysql :pool t)
          (execute-command (format nil "update channel set latest_refresh=now() where idchannel=~a" (idchannel channel)) :database db))))))

(defun refresh-channels ()
  (setf *default-pathname-defaults* (user-homedir-pathname))
  (with-database (db *db-cred* :database-type :mysql :pool t)
    (let ((idchannels (query "select
	idchannel
from
	channel
where
	date_add(latest_refresh, interval minutes_refresh_rate minute)<now()"
                             :database db :flatp t)))
      (when idchannels
        (mapcar #'refresh-channel
                (select 'channel :where [in [idchannel] idchannels] :database db :flatp t))))))
