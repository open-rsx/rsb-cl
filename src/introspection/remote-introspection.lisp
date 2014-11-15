;;;; remote-introspection.lisp --- Classes and functions for remote introspection.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This file contains the following classes:
;;;;
;;;; `introspection-receiver'
;;;;
;;;;   Receives introspection broadcast events, dispatches
;;;;   corresponding replies and performs initial introspection
;;;;   survey.
;;;;
;;;; `remote-introspection-database' and entry objects
;;;;
;;;;   Aggregates a sequence of introspection events into a sequence
;;;;   of global system states including information about hosts,
;;;;   processes and participants.
;;;;
;;;;   Remote hosts, processes and participants are represented as
;;;;   instances of the classes `participant-entry', `process-entry'
;;;;   and `host-entry'. Instances of these classes contain and manage
;;;;   instances of the associated `remote-participant-info',
;;;;   `remote-process-info' and `remote-host-info' classes.
;;;;
;;;; `remote-introspection'
;;;;
;;;;   A `participant' that aggregates introspection events received
;;;;   via an `introspection-receiver' into a
;;;;   `remote-introspection-database' instance taking care of locking
;;;;   and asynchronous updates.

(cl:in-package #:rsb.introspection)

;; `introspection-receiver'

(defclass introspection-receiver (introspection-participant-mixin
                                  rsb.ep:broadcast-processor
                                  rsb.ep:error-policy-mixin
                                  rsb.ep:error-policy-handler-mixin
                                  rsb.ep:restart-handler-mixin)
  ()
  (:documentation
   "Instances of this class receive broadcasted introspection events,
    apply filtering and error handling to deal with invalid events and
    dispatch valid events to consumers of introspection events such as
    `remote-introspection-database'."))

(rsb::register-participant-class 'introspection-receiver)

(defmethod shared-initialize :after ((instance   introspection-receiver)
                                     (slot-names t)
                                     &key)
  ;; Connect error-policy of INSTANCE to its error-hook.
  (setf (rsb.ep:processor-error-policy instance)
        (curry #'hooks:run-hook (participant-error-hook instance))))

(defmethod (setf introspection-%listener) :after ((new-value     listener)
                                                  (introspection introspection-receiver))
  ;; Install appropriate filters and install INTROSPECTION as handler.
  (pushnew *broadcast-filter* (receiver-filters new-value))
  (pushnew introspection (rsb.ep:handlers new-value)))

(defmethod introspection-server :before ((introspection introspection-receiver))
  (unless (introspection-%server introspection)
    (setf (introspection-%server introspection)
          (participant-make-child
           introspection :remote-server (introspection-hosts-scope
                                         (participant-scope introspection))
           :converters *introspection-host-converters*))))

(defmethod rsb.ep:handle ((processor introspection-receiver)
                          (data      event))
  (log:info "~@<~A received introspection response ~A~@:>" processor data)

  ;; Check scope.
  (handler-case
      (scope->participant-id (event-scope data) (participant-scope processor))
    (error (condition)
      (introspection-error
       :response data condition
       "~@<Malformed participant scope ~A~@:>"
       (scope-string (event-scope data)))))

  ;; Check payload.
  (let ((payload (event-data data)))
    (unless (or (typep payload '(or hello bye))
                (equal  payload "pong"))
      (introspection-error :response payload nil
                           "~@<Payload is ~S (not ~{~S~^ or ~})~@:>."
                           payload '(rsb.protocol.introspection:hello
                                     rsb.protocol.introspection:bye
                                     "pong"))))

  ;; Some processors want to know by which receiver an event was
  ;; received.
  (setf (meta-data data :rsb.introspection.receiver) processor)
  (rsb.ep:dispatch processor data))

(defmethod introspection-survey ((introspection introspection-receiver))
  (introspection-listener introspection) ; Force creation

  ;; Perform (initial) survey. The listener and handler configured
  ;; above will collect and process the responses. We do not have to
  ;; wait for responses since there is (almost) no difference between
  ;; requested and spontaneous responses.
  (send (introspection-informer introspection) rsb.converter:+no-value+
        :method :|survey|))

(defmethod introspection-ping ((introspection introspection-receiver)
                               (host          host-info)
                               (process       process-info)
                               &key
                               block?)
  (let+ (((&accessors-r/o (server introspection-server)) introspection)
         (scope (merge-scopes '("echo")
                              (introspection-process-scope
                               (process-info-process-id process)
                               (host-info-id host)
                               (participant-scope introspection)))))
    (call server nil (make-event scope rsb.converter:+no-value+)
          :block? block? :return :event)))

;;; Introspection objects and database
;;;
;;; The introspection database contains and manages objects which
;;; represent remote participants, processes and hosts:
;;; `participant-entry', `process-entry' and
;;; `process-entry'. Instances of these classes contain and manage
;;; instances of the associated `remote-participant-info',
;;; `remote-process-info' and `remote-host-info' classes.

;;; `info-mixin'

(defclass info-mixin ()
  ((info :initarg  :info
         :reader   entry-info
         :documentation
         "Stores the `*-info' instance associated to the entry."))
  (:default-initargs
   :info (missing-required-initarg 'info-mixin :info))
  (:documentation
   "This class is intended to be mixed into database entry classes
    that store and manage an associated `*-info' instance."))

(defmethod print-items:print-items append ((object info-mixin))
  (print-items:print-items (entry-info object)))

;;; `inactivity-threshold-mixin'

(defclass inactivity-threshold-mixin ()
  ((inactivity-threshold :initarg  :inactivity-threshold
                         :type     (or null non-negative-real)
                         :reader   introspection-inactivity-threshold
                         :initform nil
                         :documentation
                         "Stores nil or the number of seconds after
                          which inactive entries (like crashed
                          processes or unreachable hosts) should be
                          removed from the database."))
  (:documentation
   "This class is intended to be mixed into database entry classes
    instances of which should be removed from the database after a
    configurable period of inactivity."))

;;; `timing-tracking-mixin'

(defclass timing-tracking-mixin ()
  ((tracker :reader    entry-%tracker
            :initform  (make-instance 'timing-tracker)
            :documentation
            "Stores a `timing-tracker' instance which tracks
             timing-related information such as clock offset and
             latency."))
  (:documentation
   "This class is intended to be mixed into database entry classes
    instances of which track timing-related properties of their
    associated remote objects using `timing-tracker' instances. "))

(flet ((update (sink data)
         (let+ (((&accessors-r/o (info        entry-info)
                                 (tracker     entry-%tracker)
                                 (change-hook database-change-hook)) sink))
           ;; Forward DATA to TRACKER which will update its estimations.
           (rsb.ep:handle tracker data)

           ;; If TRACKER produced new clock-offset and/or latency
           ;; estimations, install the respective new value in INFO
           ;; and run CHANGE-HOOK.
           (macrolet ((update (place new event)
                        `(let ((old ,new))
                           (rotatef old ,place)
                           (unless (eql old ,place)
                             (hooks:run-hook change-hook ,place ,event)))))
             (update (info-clock-offset info)
                     (timing-tracker-clock-offset tracker)
                     :clock-offset-changed)
             (update (info-latency info)
                     (timing-tracker-latency tracker)
                     :latency-changed)))))

  (defmethod rsb.ep:handle :after ((sink timing-tracking-mixin) (data event))
    ;; Ignore ping replies.
    (unless (and (eq (event-method data) :|reply|)
                 (eq (event-data data) rsb.converter:+no-value+))
      (return-from rsb.ep:handle))

    ;; Update tracker estimates.
    (update sink data)

    ;; Record activity-tracking-mixin
    (let+ (((&structure-r/o entry- info (tracker %tracker)) sink))
      (setf (info-most-recent-activity info)
            (timing-tracker-to-local-clock tracker (timestamp data :create)))))

  (defmethod rsb.ep:handle ((sink timing-tracking-mixin) (data (eql :reset)))
    (update sink data)))

;;; `participant-entry'

(defclass participant-entry (info-mixin
                             print-items:print-items-mixin)
  ((info     :type     participant-info)
   (parent   :initarg  :parent
             :type     (or null participant-entry)
             :reader   entry-parent
             :initform nil
             :documentation
             "Stores the `participant-entry' (or
              `participant-entry-proxy') representing the parent
              participant of the participant.")
   (children :initarg  :children
             :type     list #| of participant-entry |#
             :accessor entry-children
             :initform '()
             :documentation
             "Stores a list of `participant-entry' instances
              representing the child participants of the
              participant."))
  (:documentation
   "Instances of this class represent and manage
    `remote-participant-info' instances in the hierarchy of database
    objects."))

(defmethod participant-info-id ((object participant-entry)) ; TODO avoid
  (participant-info-id (entry-info object)))

(defmethod print-items:print-items append ((object participant-entry))
  `((:num-children ,(length (entry-children object)) " (C ~D)" ((:after :transports)))))

;;; `participant-entry-proxy'

(defun make-participant-entry-proxy-info (&optional (id (uuid:make-null-uuid)))
  (make-instance 'participant-info
                 :kind  :proxy
                 :id    id
                 :scope "/"
                 :type  t))

(defclass participant-entry-proxy (participant-entry)
  ()
  (:default-initargs
   :info (make-participant-entry-proxy-info))
  (:documentation
   "Instances of this class are used when a hierarchy of
    `participant-entry's has to be built while some parents are not
    yet available."))

(defmethod shared-initialize :after ((instance   participant-entry-proxy)
                                     (slot-names t)
                                     &key
                                     id)
  (when id
    (reinitialize-instance (entry-info instance) :id id)))

;;; `process-entry'

(defclass process-entry (info-mixin
                         timing-tracking-mixin
                         participant-table-mixin
                         change-hook-mixin
                         print-items:print-items-mixin)
  ((info     :type     remote-process-info)
   (receiver :initarg  :receiver
             :reader   entry-receiver
             :documentation
             "Stores the introspection receiver through which the
              associated remote process has been discovered."))
  (:default-initargs
   :receiver (missing-required-initarg 'process-entry :receiver))
  (:documentation
   "Instances of this class represent and manage `remote-process-info'
    instances in the hierarchy of database objects. This includes
    using a `timing-tracker' instances to track clock offset and
    communication latency."))

(defmethod (setf process-info-state) ((new-value t) (info process-entry))
  (setf (process-info-state (entry-info info)) new-value))

(defmethod (setf process-info-state) :around ((new-value t) (info process-entry))
  (let ((old-value (process-info-state (entry-info info))))
    (prog1
        (call-next-method)
      (unless (eq old-value new-value)
        (hooks:run-hook (database-change-hook info) new-value :state-changed)))))

(defmethod (setf find-participant) :after ((new-value     t)
                                           (id            uuid:uuid)
                                           (introspection process-entry)
                                           &key
                                           parent-id
                                           if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (unless (or (not new-value) (typep new-value 'participant-entry-proxy))
    (hooks:run-hook (database-change-hook introspection)
                    new-value :participant-added)))

(defmethod (setf find-participant) ((new-value     (eql nil))
                                    (id            uuid:uuid)
                                    (introspection process-entry)
                                    &key
                                    parent-id
                                    if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (or (when-let ((existing (find-participant id introspection
                                             :if-does-not-exist nil)))
        ;; Remove EXISTING form its parent and remove the parent if it is
        ;; a proxy.
        (when-let ((parent (entry-parent existing)))
          (let+ (((&accessors (children  entry-children)
                              (parent-id participant-info-id)) parent))
            (removef children existing)
            (when (and (typep parent 'participant-entry-proxy)
                       (emptyp children))
              (setf (find-participant parent-id introspection) nil))))

        ;; When EXISTING still has children, turn it into a proxy.
        (when (entry-children existing)
          (change-class existing 'participant-entry-proxy
                        :info   (make-participant-entry-proxy-info id)
                        :parent nil)))

      (call-next-method)))

(defmethod (setf find-participant) :around ((new-value     (eql nil))
                                            (id            uuid:uuid)
                                            (introspection process-entry)
                                            &key
                                            parent-id
                                            if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (let ((existing (find-participant id introspection
                                    :if-does-not-exist nil)))
    (prog1
        (call-next-method)
      (when existing
        (hooks:run-hook (database-change-hook introspection)
                        existing :participant-removed)))))

(defmethod rsb.ep:handle ((sink process-entry) (data event))
  (rsb.ep:handle sink (event-data data)))

(defmethod rsb.ep:handle ((sink process-entry) (data hello))
  ;; Regarding proxies and parent-ids: for example when doing a
  ;; survey, replies for participants do not have to arrive in
  ;; topological order (or in any particular order, really). As a
  ;; result, we have to be able to store references to parents which
  ;; have not yet been added to the database. In these situations, we
  ;; insert a `participant-entry-proxy' instance which we can
  ;; `change-class' into `participant-entry' when the missing
  ;; information becomes available.

  ;; TODO move parent-id handling into (setf find-participant)?
  (let+ (((&structure-r/o
           participant-info- kind id scope type transports parent-id)
          (hello-participant data))
         (info   (make-instance 'participant-info
                                :kind       kind
                                :id         id
                                :scope      scope
                                :type       type
                                :transports transports))
         (parent (when parent-id
                   (ensure-participant
                    parent-id sink (list 'participant-entry-proxy
                                         :id parent-id))))
         (entry  (ensure-participant id sink (list 'participant-entry
                                                   :info   info
                                                   :parent parent))))
    (when parent
      (pushnew entry (entry-children parent)))
    entry))

(defmethod rsb.ep:handle ((sink process-entry) (data bye))
  (setf (find-participant (bye-id data) sink) nil))

(defmethod rsb.ep:handle ((sink process-entry) (data (eql rsb.converter:+no-value+)))
  (setf (process-info-state sink) :running))

(defmethod rsb.ep:handle ((sink process-entry) (data (eql :no-reply)))
  (setf (process-info-state sink) :crashed)
  (rsb.ep:handle sink :reset))

;;; `host-entry'

(defclass host-entry (info-mixin
                      timing-tracking-mixin
                      inactivity-threshold-mixin
                      change-hook-mixin
                      print-items:print-items-mixin)
  ((info      :type     remote-host-info)
   (processes :type     hash-table
              :accessor introspection-%processes
              :initform (make-hash-table)
              :documentation
              "Maps numeric process ids to `process-entry'
               instances."))
  (:documentation
   "Instances of this class represent and manage `remote-host-info'
    instances in the hierarchy of database objects. This includes
    using a `timing-tracker' instances to track clock offset and
    communication latency."))

(defmethod print-items:print-items append ((object host-entry))
  `((:num-processes ,(length (introspection-processes object)) " (~D)" ((:after :host-state)))))

(defmethod (setf host-info-state) ((new-value t) (info host-entry))
  (setf (host-info-state (entry-info info)) new-value))

(defmethod (setf host-info-state) :around ((new-value t) (info host-entry)) ; TODO generic info-state-mixin
  (let ((old-value (host-info-state (entry-info info))))
    (prog1
        (call-next-method)
      (unless (eq old-value new-value)
        (hooks:run-hook (database-change-hook info)
                        new-value :state-changed)))))

(defmethod introspection-processes ((host host-entry))
  (hash-table-values (introspection-%processes host)))

(defmethod find-process ((id            integer)
                         (introspection host-entry))
  (values (gethash id (introspection-%processes introspection))))

(defmethod (setf find-process) ((new-value     process-entry)
                                (id            integer)
                                (introspection host-entry))
  (setf (gethash id (introspection-%processes introspection)) new-value))

(defmethod (setf find-process) :after ((new-value     process-entry)
                                       (id            integer)
                                       (introspection host-entry))
  (hooks:run-hook (database-change-hook introspection)
                  new-value :process-added))

(defmethod (setf find-process) ((new-value     (eql nil))
                                (id            integer)
                                (introspection host-entry))
  (remhash id (introspection-%processes introspection)))

(defmethod (setf find-process) :around ((new-value     (eql nil))
                                        (id            integer)
                                        (introspection host-entry))
  (let ((process (find-process id introspection)))
    (prog1
        (call-next-method)
      (when process
        (hooks:run-hook (database-change-hook introspection)
                        process :process-removed)))))

(defmethod ensure-process ((id            integer)
                           (introspection host-entry)
                           (process       process-info)
                           &key
                           (receiver (missing-required-argument :receiver)))
  (let+ (((&structure-r/o
           process-info-
           process-id program-name commandline-arguments
           start-time executing-user)
          process)
         (transports (transport-specific-urls receiver))
         ((&flet update-transports (entry)
            (unionf (process-info-transports (entry-info entry))
                    transports :test #'puri:uri=)
            entry))
         ((&flet make-info ()
            (make-instance 'remote-process-info
                           :process-id            process-id
                           :program-name          program-name
                           :commandline-arguments commandline-arguments
                           :start-time            (timing-tracker-to-local-clock
                                                   (entry-%tracker introspection)
                                                   start-time)
                           :executing-user        executing-user
                           :state                 :running
                           :transports            transports))))
    (if-let ((entry (find-process id introspection)))
      (update-transports entry)
      (setf (find-process id introspection)
            (make-instance 'process-entry
                           :info     (make-info)
                           :receiver receiver)))))

(defmethod rsb.ep:handle ((sink host-entry) (data event))
  (when-let ((sinks (if (typep (event-data data) 'hello)
                        (let ((process  (hello-process (event-data data)))
                              (receiver (meta-data data :rsb.introspection.receiver)))
                          (list (ensure-process
                                 (process-info-process-id process) sink process
                                 :receiver receiver)))
                        (rsb.ep:handle sink (event-data data)))))
    (rsb.ep:handle sinks data)))

(defmethod rsb.ep:handle ((sink host-entry) (data bye))
  (let ((id (bye-id data)))
    (dolist (process (introspection-processes sink))
      (when (find-participant id process :if-does-not-exist nil)
        (return-from rsb.ep:handle (list process))))))

;; generic update after any event
(defmethod rsb.ep:handle :after ((sink host-entry) (data t))
  (when (eq data :reset)
    (return-from rsb.ep:handle))

  (let+ (((&structure introspection- processes inactivity-threshold) sink)
         ((&flet process-running? (entry)
            (eq (process-info-state (entry-info entry)) :running)))
         ((&flet remove-process (entry)
            (let ((id (process-info-process-id (entry-info entry))))
              (setf (find-process id sink) nil)))))

    ;; Remove processes which have no remaining participants or have
    ;; been in :crashed state for longer than the inactivity threshold
    ;; (if not nil).
    (dolist (process processes)
      (cond
        ((emptyp (introspection-participants process))
         (remove-process process))
        ((and (not (process-running? process))
              (> (info-most-recent-activity-difference (entry-info process))
                 inactivity-threshold))
         (remove-process process))))

    ;; After updating and potentially removing processes, compute the
    ;; new effective state of SINK.
    (let ((new (if (some #'process-running? processes) :up :unknown)))
      (setf (host-info-state sink) new)
      (when (eq new :unknown)
        (rsb.ep:handle sink :reset))))) ; reset tracker

(defmethod rsb.ep:handle ((sink host-entry) (data t)) ; TODO avoidable?
  (when (eq data :reset)
    (call-next-method)))

;;; `remote-introspection-database'

(defclass remote-introspection-database (inactivity-threshold-mixin
                                         change-hook-mixin
                                         print-items:print-items-mixin)
  ((hosts :type     hash-table #| key: host-id value: host-entry |#
          :reader   introspection-%hosts
          :initform (make-hash-table :test #'equal)
          :documentation
          "Stores a mapping of host ids to `host-entry' instances."))
  (:documentation
   "Instances of this class aggregate introspection events
    from (possibly remote) processes into a global, hierarchical model
    of the system consisting of hosts, processes and participants."))

(defmethod print-items:print-items append ((object remote-introspection-database))
  `((:num-hosts        ,(length (introspection-hosts object))        " (H ~D)")
    (:num-participants ,(length (introspection-participants object)) " (P ~D)")))

(defmethod introspection-participants ((introspection remote-introspection-database))
  (mappend #'introspection-participants (introspection-processes introspection)))

(defmethod introspection-processes ((introspection remote-introspection-database))
  (mappend #'introspection-processes (introspection-hosts introspection)))

(defmethod find-participant ((id            uuid:uuid)
                             (introspection remote-introspection-database)
                             &key
                             parent-id
                             if-does-not-exist)
  (declare (ignore parent-id))
  (or (some (lambda (process)
              (find-participant id process :if-does-not-exist nil))
            (introspection-processes introspection))
      (error-behavior-restart-case
          (if-does-not-exist
           (no-such-participant-error :container introspection :id id)))))

(defmethod introspection-hosts ((introspection remote-introspection-database))
  (hash-table-values (introspection-%hosts introspection)))

(defmethod find-host ((id            string)
                      (introspection remote-introspection-database))
  (values (gethash id (introspection-%hosts introspection))))

(defmethod (setf find-host) ((new-value     host-entry)
                             (id            string)
                             (introspection remote-introspection-database))
  (setf (gethash id (introspection-%hosts introspection)) new-value))

(defmethod (setf find-host) :after ((new-value     host-entry)
                                    (id            string)
                                    (introspection remote-introspection-database))
  (hooks:run-hook (database-change-hook introspection) new-value :host-added))

(defmethod ensure-host ((id            string)
                        (introspection remote-introspection-database)
                        (host          host-info))
  (let+ (((&structure-r/o introspection- inactivity-threshold) introspection)
         ((&structure-r/o host-info- (id1 id) hostname
                                     machine-type machine-version
                                     software-type software-version)
          host))
    (or (find-host id introspection)
        (setf (find-host id introspection)
              (make-instance
               'host-entry
               :info                 (make-instance
                                      'remote-host-info
                                      :id               id1
                                      :hostname         hostname
                                      :machine-type     machine-type
                                      :machine-version  machine-version
                                      :software-type    software-type
                                      :software-version software-version
                                      :state            :up)
               :inactivity-threshold inactivity-threshold)))))

(defmethod rsb.ep:handle ((sink remote-introspection-database) ; TODO mixin
                          (data event))
  (rsb.ep:handle (rsb.ep:handle sink (event-data data)) data))

(defmethod rsb.ep:handle ((sink remote-introspection-database)
                          (data hello))
  (let ((host (hello-host data)))
    (list (ensure-host (host-info-id host) sink host))))

(defmethod rsb.ep:handle ((sink remote-introspection-database)
                          (data bye))
  (introspection-hosts sink))

(defmethod rsb.ep:handle ((sink remote-introspection-database)
                          (data string))
  '()) ; ignore "pong" replies

;;; `remote-introspection'

(defclass remote-introspection (print-items:print-items-mixin
                                participant
                                lockable-database-mixin)
  ((database         :reader   introspection-database
                     :accessor introspection-%database
                     :initform nil ; for early `print-items' calls
                     :documentation
                     "Stores the database of known information about
                      remote participants.")
   (receivers        :accessor introspection-%receivers
                     :initform '()
                     :documentation
                     "Stores an introspection event receiver which
                      receives introspection events from remote
                      processes and feeds them into the database.")
   (executor         :accessor introspection-%executor
                     :initform nil ; for early `print-items' calls
                     :documentation
                     "Stores a `timed-executor' responsible for
                      running periodic updates in which the database
                      actively inquires remote processes.")
   (response-timeout :initarg  :response-timeout
                     :type     positive-real
                     :reader   introspection-response-timeout
                     :documentation
                     "Time in seconds within which an introspection
                      response has to arrive for a remote process not
                      to be considered crashed."))
  (:default-initargs
   :update-interval      10
   :response-timeout     .5
   :inactivity-threshold 60)
  (:documentation
   "Instances of this class track participants in remote processes,
    potentially on remote hosts."))

(rsb::register-participant-class 'remote-introspection)

(defmethod make-participant-using-class :around
    ((class     class)
     (prototype remote-introspection)
     (scope     scope)
     &rest args &key
     (transports (transport-options)))
  (apply #'call-next-method class prototype scope
         :transports transports args))

(defmethod initialize-instance :before ((instance remote-introspection)
                                        &key
                                        (receiver-uris nil receiver-uris-supplied?)
                                        (transports    nil transports-supplied?))
  (declare (ignore receiver-uris transports))
  (unless (or receiver-uris-supplied? transports-supplied?)
    (missing-required-initarg 'remote-introspection
                              :transports-or-receiver-uris)))

(defmethod initialize-instance :after ((instance remote-introspection)
                                       &key
                                       scope
                                       receiver-uris
                                       transports
                                       update-interval
                                       inactivity-threshold
                                       change-handler)
  (let+ (((&accessors-r/o (error-hook participant-error-hook)) instance)
         ((&accessors (change-hook database-change-hook)
                      (database    introspection-%database)
                      (receivers   introspection-%receivers)
                      (executor    introspection-%executor))
          instance))
    ;; Initialize change hook and database first. This has to be ready
    ;; when receivers are initialized to not miss results of the
    ;; initial survey.
    (setf database (make-instance 'remote-introspection-database
                                  :inactivity-threshold inactivity-threshold))
    (when change-handler
      (hooks:add-to-hook change-hook change-handler))

    ;; Initialize receivers based on RECEIVER-URIS and TRANSPORTS.
    (let+ (((&labels prepare-uri (uri)
              (etypecase uri
                (string
                 (prepare-uri (puri:parse-uri uri)))
                (puri:uri
                 (list +introspection-scope+
                       (nth-value 1 (uri->scope-and-options uri))))
                (scope
                 (list +introspection-scope+ transports)))))
           ((&labels+ make-receiver ((scope transports))
              (make-participant
               :introspection-receiver scope
               :transports   transports
               :error-policy (lambda (condition)
                               (hooks:run-hook error-hook condition))
               :handlers     (list (curry #'rsb.ep:handle instance)))))
           ((&labels add-receiver (spec)
              (push (make-receiver spec) receivers))))
      ;; Try to batch-create all requested receivers, cleaning up if
      ;; something goes wrong.
      (unwind-protect-case ()
          (progn
            (mapc #'add-receiver (if receiver-uris
                                     (mapcar #'prepare-uri receiver-uris)
                                     `((,scope ,transports))))
            ;; If all receivers have been created, start initial
            ;; surveys.
            (mapc #'introspection-survey receivers)

            ;; Initialize executor for timed updates.
            (when update-interval
              (setf executor (make-instance
                              'timed-executor/weak
                              :name     "Remote introspection update"
                              :interval update-interval
                              :function #'rsb.ep:handle
                              :args     (list instance :update)))))
        (:abort
         (detach instance))))))

(defmethod detach ((participant remote-introspection))
  (when-let ((executor (introspection-%executor participant)))
    (detach executor))
  (mapc #'detach (introspection-%receivers participant)))

(defmethod transport-specific-urls ((component remote-introspection))
  (mappend #'transport-specific-urls (introspection-%receivers component)))

(defmethod print-items:print-items append ((object remote-introspection))
  `((:name nil "") ; Suppress printing of the executor's :name item
    ,@(mappend #'print-items:print-items
               (list (introspection-database object)
                     (introspection-%executor object)))))

(defmethod print-object ((object remote-introspection) stream)
  (print-unreadable-object (object stream :type t)
    (print-items:format-print-items stream (print-items:print-items object))))

(defmethod database-change-hook ((database remote-introspection))
  (database-change-hook (introspection-database database)))

(defmethod rsb.ep:handle ((sink remote-introspection) (data event))
  (with-database-lock (sink)
    (rsb.ep:handle (introspection-database sink) data)))

(defmethod rsb.ep:handle ((sink remote-introspection) (data (eql :update)))
  (let+ (((&structure-r/o introspection- database response-timeout) sink)
         (futures '()))
    ;; Lock THING and asynchronously call "echo" methods of all known
    ;; processes.
    (with-database-lock (sink)
      (dolist (host (introspection-hosts database))
        (dolist (process (introspection-processes host))
          (let ((future (introspection-ping (entry-receiver process)
                                            (entry-info host)
                                            (entry-info process))))
            (push (list host process future) futures)))))
    ;; Give in-progress calls some time to complete.
    (sleep response-timeout)
    ;; Gather results of "echo" calls assuming processes are
    ;; non-replying if their replies did not arrive by now.
    (with-database-lock (sink)
      (mapc (lambda+ ((host process future))
              (let ((result (or (when (future-done? future)
                                  (future-result future :error? nil))
                                :no-reply)))
                (rsb.ep:handle process result)
                (rsb.ep:handle host result)))
            futures))))
