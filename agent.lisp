(in-package :guessing)

(defclass gg-agent (agent)
    ;id, experiment, discourse role, utterance, communicated-successfully
  ((categorizers
  	:documentation "Categorizers of the agent"
  	:type (or nil list)
  	:initform nil
  	:accessor categorizers)
   (memory 
    :documentation "The word meaning mappings of the agent"
    :type (or nil list)
    :initform nil
    :accessor memory)
   (age
   	:documentation "Age of the agent"
   	:accessor age
   	:initform 0
   	:type number)
   (topic
    :documentation "The true topic"
    :accessor topic
    :initform nil
    :type (or nil list))
   (applied-node
    :documentation "Node applied for categorisation"
    :accessor applied-node
    :initform nil
    :type (or nil 'cat-node))
   (applied-triple
    :documentation "Applied form-meaning triple"
    :accessor applied-triple
    :initform nil
    :type (or nil 'fmtriple)))
  (:documentation "Core of guessing-game agent"))

(defmethod initialize-instance :after ((agent gg-agent) &key)
    (let ((channels (list 'GREY 'HEIGHT 'WIDTH 'Y 'X))
          (result '()))
        (loop for i upto (- (length channels) 1)
              for root = (make-instance 'cat-node :channel-name (nth i channels))
              do (setf result (cons root result)))
        (setf (categorizers agent) result)))

;; Initialize the agent for a game

(defmethod initialize-agent ((agent gg-agent) &key topic)
    (setf (topic agent) topic)
    (setf (applied-node agent) nil)
    (setf (applied-triple agent) nil)
    (setf (communicated-successfully agent) nil))

;; UPDATE AGENT AGE
(defgeneric update-agent-age (agent)
    (:documentation "Increment the age of the agent"))

(defmethod update-agent-age ((agent gg-agent))
    (incf (age agent)))

;; UPDATE AGE OF CATEGORIZERS
(defgeneric update-categorizer-age (agent)
    (:documentation "Increment the age of all nodes in categorizers"))

(defmethod update-categorizer-age ((agent gg-agent))
    (let ((categorizers (categorizers agent)))
        (loop for cat in categorizers
              do (tree-age cat))))

;; UPDATE USE OF CATEGORIZERS
(defgeneric update-categorizer-use (agent)
    (:documentation "Increment the use of the used categorizer"))

(defmethod update-categorizer-use ((agent gg-agent))
    (let ((node (applied-node agent)))
        (incf (use node))))

;; UPDATE SUCCESS OF CATEGORIZERS
(defgeneric update-categorizer-success (agent)
    (:documentation "Increment the success of the used categorizer"))

(defmethod update-categorizer-success ((agent gg-agent))
    (let ((node (applied-node agent)))
        (incf (success node))))

;; AGENT MEMORY
(defclass fmtriple ()
  ((meaning
    :accessor meaning
    :initarg :meaning
    :initform (error "Provide a meaning when intialising a FMTriple")
    :type list)
  (form
    :accessor form
    :initarg :form
    :initform (error "Provide a form when intialising a FMTriple")
    :type string)
  (score
    :accessor score
    :initform 0.5
    :type float)))

(defun inc-score (triple inc)
  (incf (score triple) inc)
  (when (> (score triple) 1.0)
    (setf (score triple) 1.0))
  triple)

(defun dec-score (agent triple dec)
  (decf (score triple) dec)
  (when (<= (score triple) 0.0)
        (setf (memory agent) (remove triple (memory agent) :test #'equal)))
  (memory agent))

(defun find-best-meaning (agent topic)
  (the-biggest #'score (find-all topic (memory agent) :key #'meaning :test #'equal)))

(defun find-best-form (agent form)
  (the-biggest #'score (find-all form (memory agent) :key #'form :test #'equal)))

(defun insert-triple (agent topic form)
  (let ((triple (make-instance 'fmtriple :meaning topic :form form)))
    (push triple (memory agent))
    triple))

(defun get-form-competitors (agent triple)
  (remove triple (find-all (meaning triple) (memory agent) :key #'meaning :test #'equal)))

(defun get-meaning-competitors (agent triple)
  (remove triple (find-all (form triple) (memory agent) :key #'form :test #'equal)))

;; SPEAKER SIDE
(defgeneric give-form (agent topic context channels)
    (:documentation "Categorise the given topic within the context. Channels contains information about
        which channels to consider with respect to saliency. If a category is found, give or produce
        a form for this category"))

(defmethod give-form ((agent gg-agent) (topic list) (context list) (channels list))
    (let ((outcome (categorise agent topic context channels)))
        (if outcome
            (let ((triple (produce agent topic)))
                (setf (applied-triple agent) triple)
                (form triple))
            nil)))

;; PRDUCE: FIND OR CREATE A FORM FOR THE GIVEN MEANING
(defgeneric produce (agent topic)
    (:documentation "Find or create a form for the given meaning"))

(defmethod produce ((agent gg-agent) (topic list))
    (let* ((triple (find-best-meaning agent topic)))
        (if (null triple)
            (let* ((new-form (make-new-word))
                   (new-triple (insert-triple agent topic new-form)))
                new-triple)
            triple)))

;; CATEGORISATION
;; SIDE EFFECT: SETS THE APPLIED-NODE
(defgeneric categorise (agent topic context channels)
  (:documentation "Categorise the given topic with respect to the context"))

(defmethod categorise ((agent gg-agent) (topic list) (context list) (channels list))
  (let ((categorizers (categorizers agent))
        (results nil))
    (loop for i upto (- (length topic) 1)
          for categorizer = (nth i categorizers)
          when (member (channel-name categorizer) channels)
            do (let* ((topic-val (nth i topic))
                      (scene-vals (mapcar (lambda (obj) (nth i obj)) context))
                      (result-node (tree-run categorizer topic-val scene-vals)))
                    (when (not (null result-node))
                        (setf results (cons result-node results)))))
    (when (not (null results))
        (let* ((experiment (experiment agent))
               (strategy (get-configuration experiment :node-selection-strategy)))
            (setf (applied-node agent) (select-best agent results strategy))
            t))))

(defun tree-run (node topic-val scene-vals)
  (let* ((min (min-val node))
         (max (max-val node))
         (half (abs (float (/ (+ max min) 2)))))
    (if (not (null scene-vals))
        (cond
          ((and (< topic-val half) (>= topic-val min) (not (null (left node))))
            (progn
              (setf scene-vals (remove-if (lambda (x) (or (>= x half) (< x min))) scene-vals))
              (tree-run (left node) topic-val scene-vals)))
          ((and (>= topic-val half) (<= topic-val max) (not (null (right node))))
            (progn
              (setf scene-vals (remove-if (lambda (x) (or (< x half) (> x max))) scene-vals))
              (tree-run (right node) topic-val scene-vals)))
          (t nil))
        node)))

;; SELECT BEST
(defgeneric select-best (agent results strategy)
    (:documentation "Select the best node for categorisation, based on the strategy"))

(defmethod select-best ((agent gg-agent) (results list) (strategy (eql :most-use)))
    (if (= (length results) 1)
        (first results)
        (the-biggest #'use results)))

(defmethod select-best ((agent gg-agent) (results list) (strategy (eql :most-success)))
    (if (= (length results) 1)
        (first results)
        (the-biggest #'success results)))

(defmethod select-best ((agent gg-agent) (results list) (strategy (eql :depth)))
    (if (= (length results) 1)
        (first results)
        (the-smallest #'depth results)))

;; GROWTH
(defgeneric growth (agent)
    (:documentation "Grow a random categorizer"))

(defmethod growth ((agent gg-agent))
    (let* ((select (random 5))
           (categorizer (nth select (categorizers agent))))
        (grow categorizer)))

;; PRUNING
(defgeneric pruning (agent strategy)
  (:documentation "Prune the agents categorizers"))

(defmethod pruning ((agent gg-agent) (strategy (eql :off)))
    nil)

(defmethod pruning ((agent gg-agent) (strategy (eql :on)))
    (let ((categorizers (categorizers agent))
          (pruning-age (get-configuration (experiment agent) :pruning-age-threshold))
          (pruning-use (get-configuration (experiment agent) :pruning-use-threshold))
          (pruning-suc (get-configuration (experiment agent) :pruning-success-threshold)))
        (loop for categorizer in categorizers
              do (prune categorizer pruning-age pruning-use pruning-suc))))

;; HEARER SIDE
(defgeneric give-meaning (agent form)
    (:documentation "Find a meaning for the form given by the speaker"))

(defmethod give-meaning ((agent gg-agent) (form string))
    (let ((triple (find-best-form agent form)))
        (if triple
            (progn (setf (applied-triple agent) triple)
                   (meaning triple))
            nil)))

;; LEARN A NEW FORM-MEANING COMBO
(defgeneric learn-topic (agent form topic context channels)
    (:documentation "Categorise the topic and store the new form-meaning combination"))

(defmethod learn-topic ((agent gg-agent) (form string) (topic list) (context list) (channels list))
    (let ((outcome (categorise agent topic context channels)))
        (if outcome
            (insert-triple agent topic form)
            nil)))


