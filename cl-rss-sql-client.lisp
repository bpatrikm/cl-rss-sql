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

(in-package :cl-rss-sql-client)
(file-enable-sql-reader-syntax)

(defclass browse-item-base (tree-row)
  ((channel-name :initarg :channel-name :reader channel-name)
   (title :initarg :title :reader title)
   (published :initarg :published :reader published)
   (latest-view :initarg :latest-view :accessor latest-view)
   (fresh-p :initarg :fresh-p :accessor fresh-p)
   (desc :initarg :desc :reader desc)))

(defclass browse-item (browse-item-base)
  ((idpublication :initarg :idpublication :reader idpublication)
   (url :initarg :url :initform nil :reader url);för podcasts
   (site :initarg :site :reader site)))

;;vid minsta förändring i något värde i browse-item, filtren kan slå helt annorlunda, så de måste återappliceras på db-resultatet, följt av en diff-träd-process
;;kopiering sker innan trädstrukturen bildats, så bara denna klass har metod
(defmethod browse-tree-copy ((bi browse-item))
  (make-instance 'browse-item
                 :channel-name (channel-name bi)
                 :title (title bi)
                 :published (published bi)
                 :latest-view (latest-view bi)
                 :fresh-p (fresh-p bi)
                 :desc (desc bi)
                 :idpublication (idpublication bi)
                 :url (url bi)
                 :site (site bi)))

(defmethod background ((bi browse-item-base))
  ;;https://en.wikipedia.org/wiki/X11_color_names
  (if (latest-view bi)
      "grey95"
      (if (fresh-p bi)
          "azure"
          "white")))

(defmethod get-tree-children ((bi browse-item))
  nil)

(defmethod browse-tree-equal (a b)
  nil)

(defmethod browse-tree-equal ((a browse-item) (b browse-item))
  (equal (idpublication a) (idpublication b)))

(defclass coll-browse-item (browse-item-base)
  ((ident :initarg :ident :reader ident)
   (children :initarg :children :reader get-tree-children)))
(defclass coll-browse-proxy (coll-browse-item)
  ((proxied :initarg :proxied :reader proxied :type 'browse-item)))

(defmethod browse-tree-equal ((a coll-browse-item) (b coll-browse-item))
  (eql (ident a) (ident b)))

;;utgår från att published alltid är satt - att channel-name inte matchar på någon coll-browse-item
(defun channel-collapse-filter (dw-items channel-name)
  (let* ((channel-items (remove-if-not (lambda (dw-item)
                                         (equal (channel-name dw-item) channel-name))
                                       dw-items))
         (bypass-items (set-difference dw-items channel-items)))
    (let ((latest-channel-item (car (sort (copy-seq (or (remove-if #'latest-view channel-items)
                                                        channel-items))
                                          #'string> :key #'published))))
      (cons (make-instance 'coll-browse-proxy
                           :ident (intern channel-name)
                           :channel-name (format nil "~a (~a)" channel-name (length (remove-if #'latest-view channel-items)))
                           :title (or (when latest-channel-item (title latest-channel-item)) "")
                           :desc (or (when latest-channel-item (desc latest-channel-item)) "")
                           :published (when latest-channel-item (published latest-channel-item))
                           :latest-view (when latest-channel-item (latest-view latest-channel-item))
                           :fresh-p (when latest-channel-item (fresh-p latest-channel-item))
                           :proxied latest-channel-item
                           :children channel-items)
            bypass-items))))

(defun apply-filter-chain (browse-filters dw-items)
  (let ((dw-items dw-items))
    (dolist (browse-filter browse-filters)
      (setf dw-items (funcall browse-filter dw-items)))
    dw-items))

;;allt inom channel-names går till samma hop-samling, inom den blir varje regex en grupp och resten en slask, inom varje sådan presenteras utifrån äldsta osedda, en regex innehåller en parametermatch på episod-numret och appliceras på title
;;Kan generaliseras, behöver då få ett annat id än '20-min' (kolla alla ident)
(defun 20-min-filter (browse-items channel-names regexes)
  (let* ((20-min-items (remove-if-not (lambda (browse-item)
                                        (find (channel-name browse-item) channel-names :test #'equal))
                                      browse-items))
         top-latest-published
         (top-no-groups-with-unwatched 0)
         (bypass-items (set-difference browse-items 20-min-items)))
    (labels ((group-by-watched (ep-num-items ident-ind regex)
               ;;denna fungerar nästan som en filterfunktion (fast varje in-element är (cons ep-num browse-item))
               ;;behöver inte bypass, returvärde är inte lista, men en coll-browse-item
               ;;denna används på de browse-items som matchar en enskild regex
               ;;redan sedda kan samlas ihop till en egen grupp, ifall det finns osedda
               ;;den äldsta osedda (enligt num) används internt som försättsblad ifall det finns (annars "no unseen")
               ;;senaste published-sträng skickas som sekundärvärde, eftersom senaste published skall användas som övergripande published
               (let* ((ep-num-unwatcheds (remove-if #'latest-view ep-num-items :key #'cdr))
                      (latest-published (car (sort (mapcar #'published (mapcar #'cdr ep-num-items)) #'string>)))
                      (watcheds (set-difference (mapcar #'cdr ep-num-items) (mapcar #'cdr ep-num-unwatcheds))))
                 (unless (and top-latest-published
                              (string> top-latest-published latest-published))
                   (setf top-latest-published latest-published))
                 (if ep-num-unwatcheds
                     (let ((oldest-unwatched (cdar (sort (copy-seq ep-num-unwatcheds) #'< :key #'car))))
                       (incf top-no-groups-with-unwatched)
                       (make-instance
                        'coll-browse-item
                        :ident (intern regex)
                        :channel-name regex
                        :title (title oldest-unwatched)
                        :desc (desc oldest-unwatched)
                        :published (published oldest-unwatched)
                        :fresh-p t
                        :latest-view nil
                        :children
                        (cons (make-instance 'coll-browse-item
                                             :ident (intern (format nil "20-min-group-watched-~a" ident-ind))
                                             :channel-name "Watched"
                                             :title (format nil "~a" (length watcheds))
                                             :desc ""
                                             :published (or (car (sort (mapcar #'published watcheds) #'string>))
                                                            "")
                                             :fresh-p nil
                                             :latest-view (car (sort (mapcar #'latest-view watcheds) #'string>))
                                             :children watcheds)
                              (mapcar #'cdr ep-num-unwatcheds))))
                     (make-instance
                      'coll-browse-item
                      :ident (intern regex)
                      :channel-name regex
                      :title "Nothing unwatched"
                      :desc ""
                      :published (car (sort (mapcar #'published watcheds) #'string>))
                      :fresh-p nil
                      :latest-view (car (sort (mapcar #'latest-view watcheds) #'string>))
                      :children watcheds))))
             
             (collect-regex-group (regex untried &optional ep-num-matching non-matching)
               (if untried
                   (let* ((candidate (car untried))
                          (ep-num (register-groups-bind (ep-num-string)
                                      (regex (title candidate))
                                    (when ep-num-string
                                      (read-from-string ep-num-string)))));säkert, eftersom det garanterat bara är siffror
                     (if ep-num
                         (collect-regex-group regex (cdr untried) (cons (cons ep-num candidate) ep-num-matching) non-matching)
                         (collect-regex-group regex (cdr untried) ep-num-matching (cons candidate non-matching))))
                   (values ep-num-matching non-matching)))
             
             (collect-all-regex-groups (regexes non-matching &optional match-groupings)
               (if regexes
                   (multiple-value-bind (ep-num-matching non-matching)
                       (collect-regex-group (car regexes) non-matching)
                     (if ep-num-matching
                         (collect-all-regex-groups (cdr regexes) non-matching (cons (cons (car regexes)
                                                                                          ep-num-matching)
                                                                                    match-groupings))
                         (collect-all-regex-groups (cdr regexes) non-matching match-groupings)))
                   ;;en coll-browse-item för varje regex-gruppering, och en slask för resten (med collapsed-channel)
                   ;;oldest-unwatched skall vara 'försätts-blad' varje vardera grupp
                   (cons (make-instance 'coll-browse-item
                                        :ident :20-min-rest
                                        :channel-name "Others"
                                        :title (format nil "~a Ungrouped" (length non-matching))
                                        :published ""
                                        :latest-view nil
                                        :desc ""
                                        :fresh-p nil
                                        :children (apply-filter-chain
                                                   (mapcar (lambda (channel-name)
                                                             (lambda (dw-items)
                                                               (channel-collapse-filter dw-items channel-name)))
                                                           channel-names)
                                                   non-matching))
                         (remove
                          nil
                          (mapcar
                           (lambda (match-grouping ident-ind)
                             (when (cdr match-grouping)
                               (group-by-watched (cdr match-grouping) ident-ind (car match-grouping))))
                           match-groupings
                           (loop for i from 1 to (length match-groupings) :collecting i)))))))
      
      (cons (make-instance 'coll-browse-item
                           :children (collect-all-regex-groups regexes 20-min-items);körs först för setf av top-latest-published/top-no-groups-with-unwatched
                           :ident :20-min-top
                           :channel-name "20 minutes"
                           :title (format nil "~a Groups with unwatched" top-no-groups-with-unwatched)
                           :desc ""
                           :published (or top-latest-published "")
                           :latest-view nil
                           :fresh-p (> top-no-groups-with-unwatched 0))
            bypass-items))))

(defun make-title-collapse-filter (regex numeric-p &optional reverse-p);numeric-p=> regex har ett matchningsvärde på en siffra, som sorteras på
  ;;om något osett matchar => coll med channel=regex och title enligt försättsblad, innehållande osedda och coll-watched
  ;;om inget osett matchar => coll med channel=regex och title 'nothing', innehållande matchande (sedda)
  ;;bypass på allt som inte matchar
  ;;första värdet i returlistan är alltid en coll-browse-item, även om inget matchar
  (lambda (browse-items)
    (multiple-value-bind (match-unseen-items match-seen-items preface-item)
        (if numeric-p
            (let* ((num-item-pairs (remove nil
                                           (mapcar (lambda (browse-item)
                                                     (register-groups-bind (ep-num-string)
                                                         (regex (title browse-item))
                                                       (when ep-num-string
                                                         (cons (read-from-string ep-num-string);säkert, eftersom det garanterat bara är siffror
                                                               browse-item))))
                                                   (remove-if-not (lambda (bi) (typep bi 'browse-item))
                                                                  browse-items))))
                   (unseen-num-item-pairs (remove-if #'latest-view num-item-pairs :key #'cdr)))
              (values (mapcar #'cdr unseen-num-item-pairs)
                      (mapcar #'cdr (remove-if-not #'latest-view num-item-pairs :key #'cdr))
                      (cdar (sort unseen-num-item-pairs (if reverse-p #'> #'<) :key #'car))))
            (let* ((scanner (create-scanner regex))
                   (match-items (remove-if-not (lambda (browse-item)
                                                 (scan scanner (title browse-item)))
                                               (remove-if-not (lambda (bi) (typep bi 'browse-item))
                                                              browse-items)))
                   (match-unseen-items (remove-if #'latest-view match-items)))
              (values match-unseen-items
                      (remove-if-not #'latest-view match-items)
                      (car (sort (copy-seq match-unseen-items) (if reverse-p #'string> #'string<) :key #'published)))))
      (let ((collapse-item
             (if match-unseen-items
                 (let ((latest-published (car (sort (copy-seq match-unseen-items) #'string> :key #'published))))
                   (make-instance 'coll-browse-proxy
                                  :ident (intern regex)
                                  :channel-name regex
                                  :title (title preface-item)
                                  :desc (desc preface-item)
                                  :published (published latest-published);för att ha en riktigare plats i list-vyn
                                  :fresh-p (fresh-p preface-item)
                                  :latest-view nil
                                  :proxied preface-item
                                  :children
                                  (cons (make-instance 'coll-browse-item
                                                       :ident :watched
                                                       :channel-name "Watched"
                                                       :title (format nil "~a" (length match-seen-items))
                                                       :desc ""
                                                       :published ""
                                                       :fresh-p nil
                                                       :latest-view ""
                                                       :children match-seen-items)
                                        match-unseen-items)))
                 (let ((latest-seen-item (car (sort (copy-seq match-seen-items) #'string> :key #'latest-view))))
                   (make-instance 'coll-browse-item
                                  :ident (intern regex)
                                  :channel-name regex
                                  :title "Nothing unwatched"
                                  :desc ""
                                  :published (or (when latest-seen-item (published latest-seen-item)) "")
                                  :fresh-p nil
                                  :latest-view (or (when latest-seen-item (latest-view latest-seen-item)) "")
                                  :children match-seen-items)))))
        (cons collapse-item
              (set-difference browse-items (append match-seen-items match-unseen-items)))))))

(defun make-grand-collapse-filter (coll-name channel-filter-list);varje element är en lista där car är channel-name och rest är lista av filter-funktioner i stil med title-collapse-filter (car är coll med det insamlade, där klass visar om det har unwatched, och rest är bypass)
  ;;varifrån published/latest-view - de med unwatched avgör i första hand, senaste published bland dem och latest-view=nil
  ;;others ignoreras
  ;;om det då bara finns watched att ta från, published och latest-view från den som har senast latest-view
  (labels ((process-filters (browse-items filter-list collected)
             (if filter-list
                 (let ((result (funcall (car filter-list) browse-items)))
                   (process-filters (cdr result) (cdr filter-list) (cons (car result) collected)))
                 (values collected
                         browse-items)))
           
           (process-channels (browse-items channel-filter-list collected collected-others)
             (if channel-filter-list
                 (let* ((channel-name (car (car channel-filter-list)))
                        (channel-filters (cdr (car channel-filter-list)))
                        (channel-matches (remove-if-not (lambda (browse-item)
                                                          (equal (channel-name browse-item)
                                                                 channel-name))
                                                        browse-items)))
                   (multiple-value-bind (new-collected new-other-items)
                       (process-filters channel-matches channel-filters nil)
                     (process-channels (set-difference browse-items channel-matches)
                                       (cdr channel-filter-list)
                                       (append new-collected collected)
                                       (append (channel-collapse-filter new-other-items channel-name) collected-others))))
                 (values collected collected-others browse-items))))
    (lambda (browse-items)
      (multiple-value-bind (regex-match-colls channel-other-colls bypass)
          (process-channels browse-items channel-filter-list nil nil)
        ;;finns det någon coll-browse-proxy i regex-match-colls; det är då en med unwatched
        (multiple-value-bind (title published latest-view fresh-p)
            (let ((unwatched-colls (remove-if-not (lambda (coll)
                                                    (typep coll 'coll-browse-proxy))
                                                  regex-match-colls)))
              (if unwatched-colls
                  (let ((latest (car (sort (copy-seq unwatched-colls) #'string> :key #'published))))
                    (values (format nil "~a Groups with unwatched" (length unwatched-colls))
                            (published latest) nil t))
                  (let ((latest (car (sort (copy-seq regex-match-colls) #'string> :key #'latest-view))))
                    (values "None with unwatched"
                            (published latest) (latest-view latest) nil))))
          (cons (make-instance 'coll-browse-item
                               :ident (intern coll-name)
                               :channel-name coll-name
                               :title title
                               :desc ""
                               :published published
                               :latest-view latest-view
                               :fresh-p fresh-p
                               :children
                               (cons (make-instance 'coll-browse-item
                                                    :ident :others
                                                    :channel-name "Others"
                                                    :title ""
                                                    :desc ""
                                                    :published ""
                                                    :latest-view ""
                                                    :fresh-p nil
                                                    :children channel-other-colls)
                                     regex-match-colls))
                bypass))))))
      
(defclass browse-list (tree-layer scl-with-flex)
  ())

(defmethod channel-name-safe ((bi browse-item-base))
  (or (channel-name bi) ""))
(defmethod title-safe ((bi browse-item-base))
  (or (title bi) ""))
(defmethod published-safe ((bi browse-item-base))
  (or (published bi) ""))
(defmethod latest-view-safe ((bi browse-item-base))
  (or (latest-view bi) "Never"))

(defun make-browse-list ()
  (make-instance 'browse-list
                 :headers (list "Channel" "Title" "Published" "Saw At")
                 :column-readers (list #'channel-name-safe #'title-safe #'published-safe #'latest-view-safe #'background)
                 :column-attribute-source (list (list (cons 0 "text") (cons 4 "background"))
                                                (list (cons 1 "text") (cons 4 "background"))
                                                (list (cons 2 "text"))
                                                (list (cons 3 "text")))))

(defclass browse-viewer (tree-viewer-excl)
  ((browse-filter :initarg :browse-filter :initform #'identity :accessor browse-filter)
   (filter-handler :initarg :filter-handler :reader filter-handler)
   (db-results :initform nil :accessor db-results)
   (gtk-name-label :initarg :gtk-name-label :reader gtk-name-label)
   (gtk-title-label :initarg :gtk-title-label :reader gtk-title-label)
   (gtk-published-label :initarg :gtk-published-label :reader gtk-published-label)
   (gtk-latest-label :initarg :gtk-latest-label :reader gtk-latest-label)
   (gtk-desc-text-buffer :initarg :gtk-desc-text-buffer :reader gtk-desc-text-buffer)))

(defun make-browse-viewer (browse-list gtk-name-label gtk-title-label gtk-published-label gtk-latest-label gtk-desc-text-buffer filter-handler)
  (let ((tree-viewer (make-tree-viewer browse-list 'browse-rss 'main-table t t t)))
    (change-class tree-viewer
                  'browse-viewer
                  :single-p t
                  :filter-handler filter-handler
                  :gtk-name-label gtk-name-label
                  :gtk-title-label gtk-title-label
                  :gtk-published-label gtk-published-label
                  :gtk-latest-label gtk-latest-label
                  :gtk-desc-text-buffer gtk-desc-text-buffer)))

(defun clean-html (target-string)
  (flet ((clean-amp (target-string) (regex-replace-all "&amp;" target-string "&"));applicera denna först
         (clean-quot (target-string) (regex-replace-all "&quot;" target-string "\""))
         (clean-apo (target-string) (regex-replace-all "&#39;" target-string "'")))
    (clean-apo (clean-quot (clean-amp target-string)))))

(defun get-browse-items ()
  (with-database (db *db-cred* :database-type :mysql :pool t)
    (let ((channels (select 'channel :database db :flatp t))
          (publications (select 'publication :database db :flatp t)))
      (mapcar (lambda (publication)
                (let ((channel (find (idchannel publication) channels :key #'idchannel))
                      (edition (latest-edition (idpublication publication))))
                  (make-instance 'browse-item
                                 :channel-name (name channel)
                                 :title (clean-html (title edition))
                                 :published (published publication)
                                 :latest-view (latest-view publication)
                                 :fresh-p (fresh-p publication)
                                 :desc (clean-html (desc edition))
                                 :idpublication (idpublication publication)
                                 :url (url publication)
                                 :site (cond
                                         ((equal (site channel) "youtube") :youtube)
                                         ((equal (site channel) "bitchute") :bitchute)
                                         ((equal (site channel) "podcasts") :podcasts)
                                         (t (error "Unknown site for browse"))))))
              publications))))

;;vid uppdatering från db, fås en ny lista som helt ersätter den gamla. 

(defmethod remove-browse-sub-tree ((sub-root browse-item) (bl browse-list))
  (direct-remove-row bl sub-root))

(defmethod remove-browse-sub-tree ((sub-root coll-browse-item) (bl browse-list))
  (mapcar (lambda (child)
            (remove-browse-sub-tree child bl))
          (get-tree-children sub-root))
  (direct-remove-row bl sub-root))

(defmethod insert-browse-sub-tree ((new browse-item) parent (bl browse-list))
  (declare ((or null coll-browse-item) parent))
  (direct-insert-row bl parent new))

(defmethod insert-browse-sub-tree ((new coll-browse-item) parent (bl browse-list))
  (declare ((or null coll-browse-item) parent))
  (direct-insert-row bl parent new)
  (mapcar (lambda (child)
            (insert-browse-sub-tree child new bl))
          (get-tree-children new)))

(defmethod adjust-tree-diff (childs-old childs-new (bl browse-list) &optional parent)
  (let ((removed (set-difference childs-old childs-new :test #'browse-tree-equal))
        (added (set-difference childs-new childs-old :test #'browse-tree-equal)))
    (let ((remained-old (set-difference childs-old removed :test #'browse-tree-equal))
          (remained-new (set-difference childs-new added :test #'browse-tree-equal)))
      (mapcar (lambda (rem) (remove-browse-sub-tree rem bl)) removed)
      (mapcar (lambda (add) (insert-browse-sub-tree add parent bl)) added)
      (dolist (rem-old remained-old)
        (let ((rem-new (find rem-old remained-new :test #'browse-tree-equal)))
          (adjust-tree-diff rem-old rem-new bl))))))

(defmethod adjust-tree-diff ((rem-old browse-item) (rem-new browse-item) (bl browse-list) &optional parent)
  (declare (ignore parent))
  ;;även om det inte finns något att justera, behöver rem-new få iter från rem-old, tree-store-basic:update-row kollar huruvida vy-kolumn-strängarna skiljer sig och uppdaterar gtk-vyn därefter
  (direct-replace-row bl rem-old rem-new))

(defmethod adjust-tree-diff ((rem-old coll-browse-item) (rem-new coll-browse-item) (bl browse-list) &optional parent)
  (declare (ignore parent))
  (direct-replace-row bl rem-old rem-new)
  (adjust-tree-diff (get-tree-children rem-old) (get-tree-children rem-new) bl rem-new))

(defmethod re-sort ((v browse-viewer))
  (gtk-tree-view-column-clicked (elt (cl-gtk-frank.tree-store-basic::gtk-view-columns v) 2))
  (gtk-tree-view-column-clicked (elt (cl-gtk-frank.tree-store-basic::gtk-view-columns v) 2))
  (gtk-tree-view-column-clicked (elt (cl-gtk-frank.tree-store-basic::gtk-view-columns v) 3))
  (gtk-tree-view-column-clicked (elt (cl-gtk-frank.tree-store-basic::gtk-view-columns v) 3)))

(defun refresh-list-from-db (browse-viewer)
  (declare (browse-viewer browse-viewer))
  (setf (db-results browse-viewer) (get-browse-items))
  (let ((new-root-content (funcall (browse-filter browse-viewer) (mapcar #'browse-tree-copy (db-results browse-viewer)))))
    (adjust-tree-diff (root-content (store-content-layer browse-viewer))
                      new-root-content
                      (store-content-layer browse-viewer))
    (replace-content (store-content-layer browse-viewer)
                     new-root-content))
  (verify-selection browse-viewer)
  (re-sort browse-viewer))

(defun init-browse-list (browse-viewer)
  (declare (browse-viewer browse-viewer))
  (setf (db-results browse-viewer) (get-browse-items))
  (setf (slot-value (filter-handler browse-viewer) 'browse-viewer) browse-viewer)
  (set-filters (filter-handler browse-viewer))
  (replace-content (store-content-layer browse-viewer)
                   (funcall (browse-filter browse-viewer) (db-results browse-viewer)))
  (update-all-viewers (list (store-content-layer browse-viewer)))
  (re-sort browse-viewer))

;;vid uppdateringar av något slot-värde. ta en ny kopia av db-results, hitta browse-tree-equal i den, ändra i kopian och kör hela uppsättningen av kopior genom browse-filtret, gör sedan adjust-tree-diff
;;Notera att filter-funktionerna inte får modifiera strängarna (eller någon slot) i de browse-items de hanterar, eftersom modcopy inte kopierar upp nya strängar. (Att helt ersätta slot-värdet med ny sträng är OK som användar-interaktion, men filterfunktionerna skall inte göra det)
(defun process-modification (browse-viewer modcopy-db-results)
  (declare (browse-viewer browse-viewer))
  (setf (db-results browse-viewer) modcopy-db-results)
  (let ((new-root-content (funcall (browse-filter browse-viewer) modcopy-db-results)))
    (adjust-tree-diff (root-content (store-content-layer browse-viewer)) new-root-content (store-content-layer browse-viewer))
    (replace-content (store-content-layer browse-viewer) new-root-content)))

(defun get-db-linecount ()
  (with-database (db *db-cred* :database-type :mysql :pool t)
    (caar (query "select count(*) from publication" :database db))))

(defmethod update-panel ((bv browse-viewer) (bi browse-item-base))
  (gtk-label-set-text (gtk-name-label bv) (channel-name-safe bi))
  (gtk-label-set-text (gtk-title-label bv) (title-safe bi))
  (gtk-label-set-text (gtk-published-label bv) (published-safe bi))
  (gtk-label-set-text (gtk-latest-label bv) (latest-view-safe bi))
  (gtk-text-buffer-set-text (gtk-desc-text-buffer bv) (or (desc bi) "")))

(defmethod mark-not-fresh ((bv browse-viewer) any)
  )

(defmethod mark-not-fresh ((bv browse-viewer) (bi coll-browse-proxy))
  (mark-not-fresh bv (proxied bi)))

(defmethod mark-not-fresh ((bv browse-viewer) (bi browse-item))
  (when (fresh-p bi)
    (with-database (db *db-cred* :database-type :mysql :pool t)
      (update-records ["publication"] :av-pairs '((["fresh"] nil)) :where [= ["idpublication"] (idpublication bi)] :database db))
    (let* ((modcopy (mapcar #'browse-tree-copy (db-results bv)))
           (mod-item (find bi modcopy :test #'browse-tree-equal)))
      (unless mod-item (error "Cannot find"))
      (setf (fresh-p mod-item) nil)
      (process-modification bv modcopy))))

(defmethod changed-enriched ((bv browse-viewer) (bi browse-item-base))
  (update-panel bv bi)
  (new-current (filter-handler bv) bi)
  (mark-not-fresh bv bi))

(defclass refresh-data ()
  ((latest-linecount :initarg :latest-linecount :accessor latest-linecount);sätt till get-db-linecount vid init
   (refresh-timer :accessor refresh-timer)
   (gtk-refresh-clicker :initarg :gtk-refresh-clicker :reader gtk-refresh-clicker)))

(defun after-refresh (refresh-data)
  (setf (gtk-widget-sensitive (gtk-refresh-clicker refresh-data)) nil
        (latest-linecount refresh-data) (get-db-linecount))
  (sb-ext:schedule-timer (refresh-timer refresh-data) 60 :repeat-interval 60))

(defun refresh-timer-action (refresh-data)
  (unless (= (get-db-linecount) (latest-linecount refresh-data))
    (sb-ext:unschedule-timer (refresh-timer refresh-data))
    (within-main-loop (setf (gtk-widget-sensitive (gtk-refresh-clicker refresh-data)) t))))

(defmethod mark-view ((bv browse-viewer))
  ;;ändra i db, listan och panelen
  (let ((bi
         (let ((bi (selection bv)))
           (when bi (or (when (not (typep bi 'coll-browse-item))
                          bi)
                        (when (typep bi 'coll-browse-proxy)
                          (proxied bi)))))))
    (when bi
      (with-database (db *db-cred* :database-type :mysql :pool t)
        (update-records ["publication"]
                        :av-pairs '((["first_view"] ["now()"]))
                        :where [and [= ["idpublication"] (idpublication bi)] [null ["first_view"]]]
                        :database db)
        (update-records ["publication"]
                        :av-pairs '((["latest_view"] ["now()"]))
                        :where [= ["idpublication"] (idpublication bi)]
                        :database db)
        (let ((new-latest-view (caar
                                (select ["latest_view"]
                                        :from ["publication"]
                                        :where [= ["idpublication"] (idpublication bi)]
                                        :database db))))
          (let* ((modcopy (mapcar #'browse-tree-copy (db-results bv)))
                 (mod-item (find bi modcopy :test #'browse-tree-equal)))
            (setf (latest-view mod-item) new-latest-view)
            (process-modification bv modcopy))
          (gtk-label-set-text (gtk-latest-label bv) new-latest-view)))
      (verify-selection bv)
      (re-sort bv))))

(defmethod row-activated ((bv browse-viewer) path gtk-view-column)
  (declare (ignore path gtk-view-column))
  (let ((browse-item
         (let ((browse-item (selection bv)))
           (when browse-item
             (or (when (not (typep browse-item 'coll-browse-item))
                   browse-item)
                 (when (typep browse-item 'coll-browse-proxy)
                   (proxied browse-item)))))))
    (when browse-item
      (sb-ext:run-program "mpv"
                          (list (case (site browse-item)
                                  (:youtube (format nil "ytdl://www.youtube.com/watch?v=~a" (idpublication browse-item)))
                                  (:bitchute (format nil "ytdl://www.bitchute.com/video/~a/" (idpublication browse-item)))
                                  (:podcasts (url browse-item))))
                          :wait nil :search t))))

(defclass mark-view-clicker (widget)
  ((browse-viewer :initarg :browse-viewer :reader browse-viewer)))
(defmethod clicked ((b mark-view-clicker))
  (mark-view (browse-viewer b)))

(defclass refresh-clicker (widget)
  ((browse-viewer :initarg :browse-viewer :reader browse-viewer)
   (refresh-data :initarg :refresh-data :reader refresh-data)))
(defmethod clicked ((b refresh-clicker))
  (refresh-list-from-db (browse-viewer b))
  (after-refresh (refresh-data b)))

(defclass browse-filter ()
  ((filter-function :initarg :filter-function :reader filter-function)))

;;bara den här klassen skall vara borttagbar
(defclass browse-filter-channel (browse-filter)
  ((ident :initarg :ident :reader ident)))

(defclass filters ()
  ((channel-filters :initform nil :initarg :channel-filters :accessor channel-filters)
   (other-filters :initarg :other-filters :reader other-filters)
   (current :initform nil :accessor current)
   (browse-viewer :reader browse-viewer)
   (add-clicker :initarg :add-clicker :reader add-clicker)
   (remove-clicker :initarg :remove-clicker :reader remove-clicker)))

(defmethod process-filter-change ((bv browse-viewer))
  (let ((new-root-content (funcall (browse-filter bv) (db-results bv))))
    (adjust-tree-diff (root-content (store-content-layer bv)) new-root-content (store-content-layer bv))
    (replace-content (store-content-layer bv) new-root-content))
  (verify-selection bv)
  (re-sort bv))

(defmethod set-filters ((f filters))
  (setf (browse-filter (browse-viewer f))
        (lambda (browse-items)
          (apply-filter-chain (append (mapcar #'filter-function (other-filters f))
                                      (mapcar #'filter-function (channel-filters f)))
                              browse-items))))

(defmethod removable-active ((f filters))
  (setf (gtk-widget-sensitive (remove-clicker f))
        (and (current f)
             (typep (current f) 'coll-browse-item)
             (find (ident (current f)) (channel-filters f) :key #'ident))))

(defmethod remove-do ((f filters))
  (let ((target (find (ident (current f)) (channel-filters f) :key #'ident)))
    (setf (channel-filters f)
          (remove target (channel-filters f))))
  (setf (gtk-widget-sensitive (remove-clicker f)) nil)
  (set-filters f)
  (process-filter-change (browse-viewer f)))

(defmethod addable-active ((f filters))
  (setf (gtk-widget-sensitive (add-clicker f))
        (and (current f)
             (typep (current f) 'browse-item)
             (not (find (intern (channel-name (current f))) (channel-filters f) :key #'ident)))))

(defmethod add-do ((f filters))
  (let* ((channel-name (channel-name (current f)))
         (new (make-instance 'browse-filter-channel
                             :ident (intern (channel-name (current f)))
                             :filter-function (lambda (browse-items)
                                                (channel-collapse-filter browse-items channel-name)))))
    (push new (channel-filters f)))
  (setf (gtk-widget-sensitive (add-clicker f)) nil)
  (set-filters f)
  (process-filter-change (browse-viewer f)))

(defmethod new-current ((f filters) new-current)
  (setf (current f) new-current)
  (removable-active f)
  (addable-active f))

(defclass remove-filter-clicker (widget)
  ((filters :initarg :filters :reader filters)))
(defmethod clicked ((b remove-filter-clicker))
  (remove-do (filters b)))

(defclass add-filter-clicker (widget)
  ((filters :initarg :filters :reader filters)))
(defmethod clicked ((b add-filter-clicker))
  (add-do (filters b)))

(defparameter *channel-filters* nil)
(defparameter *other-filters* nil)

(defun start-browse-rss ()
  (within-main-loop
    (with-build-interface ((namestring (merge-pathnames "cl-rss-sql.glade" (asdf:component-pathname (asdf:find-system :cl-rss-sql)))) 'browse-rss builder t)
      (let ((toplevel (gtk-builder-get-object builder "toplevel"))
            (viewport (gtk-builder-get-object builder "table-viewport"))
            (name-label (gtk-builder-get-object builder "name-label"))
            (title-label (gtk-builder-get-object builder "title-label"))
            (published-label (gtk-builder-get-object builder "published-label"))
            (latest-label (gtk-builder-get-object builder "latest-label"))
            (desc-text-buffer (gtk-builder-get-object builder "desc-text-buffer"))
            (refresh-clicker (gtk-builder-get-object builder "refresh-clicker"))
            (add-clicker (gtk-builder-get-object builder "add-clicker"))
            (remove-clicker (gtk-builder-get-object builder "remove-clicker"))
            (css-provider (gtk-css-provider-new)))
        (gtk-css-provider-load-from-path css-provider (namestring (merge-pathnames "cl-rss-sql-client.css" (asdf:component-pathname (asdf:find-system :cl-rss-sql)))))
        (labels ((set-style-context-all (widget)
                   (gtk-style-context-add-provider (gtk-widget-get-style-context widget) css-provider 800)
                   (when (typep widget 'gtk-container)
                     (gtk-container-foreach widget #'set-style-context-all))))
          (set-style-context-all toplevel))
        (setf (gtk-widget-sensitive refresh-clicker) nil)
        (let* ((filter-handler
                (make-instance
                 'filters
                 :add-clicker add-clicker
                 :remove-clicker remove-clicker
                 :channel-filters *channel-filters*
                 :other-filters *other-filters*))
               (bv (make-browse-viewer
                    (make-browse-list)
                    name-label title-label published-label latest-label desc-text-buffer filter-handler))
               (refresh-data (make-instance 'refresh-data
                                            :latest-linecount (get-db-linecount)
                                            :gtk-refresh-clicker refresh-clicker)))
          (let ((refresh-timer (sb-ext:make-timer (lambda () (refresh-timer-action refresh-data)) :thread t)))
            (setf (refresh-timer refresh-data) refresh-timer))
          (gtk-container-add viewport (gtk-view bv))
          (init-browse-list bv)
          (sb-ext:schedule-timer (refresh-timer refresh-data) 60 :repeat-interval 60)
          (gtk-window-set-wmclass toplevel "cl-rss-sql" "cl-rss-sql")
          ;;(gtk-window-set-icon-from-file toplevel "/home/patrik/GNUstep/Library/Icons/rss-icon.png")
          (gtk-widget-show-all toplevel)
          (list (cons "mark-view-clicker" (make-instance 'mark-view-clicker :browse-viewer bv))
                (cons "refresh-clicker" (make-instance 'refresh-clicker :browse-viewer bv :refresh-data refresh-data))
                (cons "remove-clicker" (make-instance 'remove-filter-clicker :filters filter-handler))
                (cons "add-clicker" (make-instance 'add-filter-clicker :filters filter-handler))
                (cons "toplevel" (make-instance 'widget
                                                :destroy-hook (lambda ()
                                                                (sb-ext:unschedule-timer (refresh-timer refresh-data)))))))))))


(let ((filepath (merge-pathnames "cl-rss-sql-client-filters.lisp" (asdf:component-pathname (asdf:find-system :cl-rss-sql)))))
  (when (probe-file filepath)
    (load (compile-file filepath))))
