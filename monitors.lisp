
(in-package :guessing)


;; LEXICON SIZE MONITOR
(define-monitor record-lexicon-size
	:class 'data-recorder)

(define-monitor export-lexicon-size
	:class 'lisp-data-file-writer
    :documentation "Exports size of the lexicon"
    :data-sources '(record-lexicon-size)
    :file-name (babel-pathname :name "lexicon-size" :type "lisp" 
                               :directory '("examples" "guessing" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor
  				(let ((agents (population experiment))
  					  (sizes 0))
  	            	(loop for agent in agents
  	            		  for agent-forms = nil
  	            		  do (loop for triple in (memory agent)
  	            		  	       when (not (member (form triple) agent-forms))
  	            		  	       		do (setf agent-forms (cons (form triple) agent-forms)))
  	            		  do (incf sizes (length agent-forms)))
  	            	(float (/ sizes (length agents))))))

;; REPERTOIRE SIZE
(define-monitor record-repertoire-size
	:class 'data-recorder)

(define-monitor export-repertoire-size
	:class 'lisp-data-file-writer
	:documentation "Exports size of the repertoire"
	:data-sources '(record-repertoire-size)
	:file-name (babel-pathname :name "repertoire-size" :type "lisp"
		                       :directory '("examples" "guessing" "raw-data"))
	:add-time-and-experiment-to-file-name nil
	:column-separator " "
	:comment-string "#")

(defun tree-size (tree)
	(if (and (null (left tree)) (null (right tree)))
		1
		(+ 1
		   (tree-size (left tree))
		   (tree-size (right tree)))))

(define-event-handler (record-repertoire-size interaction-finished)
	(record-value monitor
				  (let ((agents (agents experiment))
				  	    (sizes 0))
				  	(loop for agent in agents
				  		  for agent-size = 0
				  		  do (loop for categorizer in (categorizers agent)
				  		  	       for size = (tree-size categorizer)
				  		  	       do (incf agent-size size))
				  		  do (incf sizes agent-size))
				  	(float (/ sizes (length agents))))))

;; DISCRIMINATIVE SUCCESS
(define-monitor record-discriminative-success
	:class 'data-recorder)

(define-monitor export-discriminative-success
	:class 'lisp-data-file-writer
	:documentation "Exports discriminative success"
	:data-sources '(record-discriminative-success)
	:file-name (babel-pathname :name "discriminative-success" :type "lisp"
		                       :directory '("examples" "guessing" "raw-data"))
	:add-time-and-experiment-to-file-name nil
	:column-separator " "
	:comment-string "#")

(define-event-handler (record-discriminative-success interaction-finished)
	(record-value monitor
		          (let ((speaker (speaker experiment)))
		          		(if (applied-node speaker)
		          			1
		          			0))))

;; COMMUNICATIVE SUCCESS
(define-monitor record-communicative-success
	:class 'data-recorder)

(define-monitor export-communicative-success
	:class 'lisp-data-file-writer
	:documentation "Exports communicative success"
	:data-sources '(record-communicative-success)
	:file-name (babel-pathname :name "communicative-success" :type "lisp"
		                       :directory '("examples" "guessing" "raw-data"))
	:add-time-and-experiment-to-file-name nil
	:column-separator " "
	:comment-string "#")

(define-event-handler (record-communicative-success interaction-finished)
	(record-value monitor
		          (let ((speaker (speaker experiment))
		          	    (hearer (hearer experiment)))
		          		(if (and (communicated-successfully speaker)
		          			     (communicated-successfully hearer))
		          			1
		          			0))))

;; LEXICAL COHERENCE
(define-monitor record-lexical-coherence
	:class 'data-recorder)

(define-monitor export-lexical-coherence
	:class 'lisp-data-file-writer
	:documentation "Exports lexical coherence"
	:data-sources '(record-lexical-coherence)
	:file-name (babel-pathname :name "lexical-coherence" :type "lisp"
		                       :directory '("examples" "guessing" "raw-data"))
	:add-time-and-experiment-to-file-name nil
	:column-separator " "
	:comment-string "#")

(defun prefered-form (agent)
  (let ((prefered (the-biggest #'score (find-all (topic agent) (memory agent) :key #'meaning :test #'equal))))
    (when prefered
    	  (form prefered))))

(define-event-handler (record-lexical-coherence interaction-finished)
  (record-value monitor 
                (if (communicated-successfully (first (interacting-agents experiment)))
                    (let* ((speaker (speaker experiment))
                           (hearer (hearer experiment))
                           (hearer-prefered-form (prefered-form hearer))
                           (hearer-prefers-same-form? (and hearer-prefered-form
                                                           (equal hearer-prefered-form
                                                                  (form (applied-triple speaker))))))
                      (if hearer-prefers-same-form? 
                      	  1 
                      	  0))
                    0)))

;; NUMBER OF AGENTS
(define-monitor record-population-size
	:class 'data-recorder)

(define-monitor export-population-size
	:class 'lisp-data-file-writer
	:documentation "Exports population size"
	:data-sources '(record-population-size)
	:file-name (babel-pathname :name "population-size" :type "lisp"
		                       :directory '("examples" "aipp-guessing-game" "raw-data"))
	:add-time-and-experiment-to-file-name nil
	:column-separator " "
	:comment-string "#")

(define-event-handler (record-population-size interaction-finished)
	(record-value monitor
		          (get-configuration experiment :population-size)))

;; NUMBER OF OBJECTS
(define-monitor record-scene-size
	:class 'data-recorder)

(define-monitor export-scene-size
	:class 'lisp-data-file-writer
	:documentation "Exports scene size"
	:data-sources '(record-scene-size)
	:file-name (babel-pathname :name "scene-size" :type "lisp"
		                       :directory '("examples" "aipp-guessing-game" "raw-data"))
	:add-time-and-experiment-to-file-name nil
	:column-separator " "
	:comment-string "#")

(define-event-handler (record-scene-size interaction-finished)
	(record-value monitor
		          (length (world experiment))))



