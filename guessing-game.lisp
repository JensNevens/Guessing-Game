
(in-package :guessing)

;; Experiment has slots ID, agents, series-number, world, interactions
(defclass gg-experiment (experiment)
	()
	(:documentation "Class for Guessing Game experiment"))

;; INITIALIZE WORLD AND AGENTS IN EXPERIMENT
(defmethod initialize-instance :after ((experiment gg-experiment) &key)
  	(setf (world experiment) (loop with data = (read-data-file)
  		                           repeat (get-configuration experiment :world-size)
		                           collect (nth (random (length data)) data)))
  	(setf (agents experiment) (loop for x from 1 to (get-configuration experiment :population-size)
		                            collect (make-instance 'gg-agent :id x :experiment experiment))))

;; DEFAILT CONFIG VALUES
(define-configuration-default-value :population-size 2)
(define-configuration-default-value :world-size 5)
(define-configuration-default-value :agent-influx :none) ;can be :new, :replace, :none
(define-configuration-default-value :agent-influx-rate 500)
(define-configuration-default-value :object-influx nil) ;can be t or nil
(define-configuration-default-value :object-influx-rate 500)
(define-configuration-default-value :channels (list 'X 'Y 'WIDTH 'HEIGHT 'GREY))
(define-configuration-default-value :saliency-strategy :most-salient) ;can be :most-salient or :above-threshold
(define-configuration-default-value :saliency-threshold 0)
(define-configuration-default-value :node-selection-strategy :depth) ;can be :most-use, :most-success or :depth
(define-configuration-default-value :alignment-strategy :li-1) ;li=lateral inhibition
(define-configuration-default-value :li-inc 0.1)
(define-configuration-default-value :li-dec 0.2)
(define-configuration-default-value :pruning-strategy :off) ;can be :on or :off
(define-configuration-default-value :pruning-age-threshold 20)
(define-configuration-default-value :pruning-success-threshold 0.05)
(define-configuration-default-value :pruning-use-threshold 10)

;; READ DATA FILE
(defun read-data-file ()
	(let ((in (open "examples/guessing/qrio-1/object-features.txt"))
		  (data '()))
		(read-line in)
		(when in
			(loop for line = (read-line in nil)
				  while line do (setf data (cons (filter-channels (my-split line)) data)))
			(close in))
		data))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (read-from-string (string-trim " " (subseq string beg end)))
    :while end))

(defun delimiterp (c) (char= c #\,))

(defun filter-channels (lst)
	(let ((channels '(0 1 3 4 5)))
		(loop for i in channels
			  collect (nth i lst))))

;; COMPUTE SALIENCY AND CONTEXT SCALE DATA
(defun saliency (topic scenes)
	(let ((result (list nil nil nil nil nil)))
		(loop for scene in scenes
			  do (loop for i from 0 to (- (length topic) 1)
			  	       for scene-val = (nth i scene)
			  	       for topic-val = (nth i topic)
			  	       for saliency-val = (abs (- scene-val topic-val))
			  	       when (null (nth i result))
			  	       	do (setf (nth i result) saliency-val)
			  	       when (< saliency-val (nth i result))
			  	       	do (setf (nth i result) saliency-val))
			  finally (return result))))

(defun context-scaling (topic scenes)
	(let ((topicc (copy-tree topic))
		  (scenesc (copy-tree scenes)))
		(loop for i from 0 to (- (length topic) 2)
			  for top-val = (nth i topic)
			  for scene-vals = (mapcar (lambda (x) (nth i x)) scenes)
			  do (multiple-value-bind (tp sc) (scale top-val scene-vals)
			  		(progn 
			  			(setf (nth i topicc) tp)
			  			(loop for j from 0 to (- (list-length sc) 1)
			  	       		  do (setf (nth i (nth j scenesc)) (nth j sc)))))
		      finally (return (values topicc scenesc)))))

(defun scale (topic scene)
	(let* ((minm (min topic (apply #'min scene)))
		   (maxm (max topic (apply #'max scene)))
		   (top (/ (- topic minm) (- maxm minm))))
		(loop for val in scene
			  collect (/ (- val minm) (- maxm minm)) into scaled-scene
			  finally (return (values top scaled-scene)))))

;; SELECT CHANNELS BASED ON SALIENCY
(defgeneric select-channels (experiment saliency strategy)
	(:documentation "Select channels to use based on saliency-strategy"))

(defmethod select-channels ((experiment gg-experiment) (saliency list) (strategy (eql :most-salient)))
	(let ((channels (get-configuration experiment :channels))
		  (saliency-value -1)
		  (best-channel nil))
		(loop for i upto (- (length saliency) 1)
			  for sal-val = (nth i saliency)
			  when (> sal-val saliency-value)
			  	do (progn
			  			(setf saliency-value sal-val)
			  	 		(setf best-channel (nth i channels))))
		(list best-channel)))

(defmethod select-channels ((experiment gg-experiment) (saliency list) (strategy (eql :above-threshold)))
	(let ((channels (get-configuration experiment :channels))
		  (threshold (get-configuration experiment :saliency-threshold))
		  (best-channels nil))
		(loop for i upto (- (length saliency) 1)
			  for sal-val = (nth i saliency)
			  when (> sal-val threshold)
			  	do (setf best-channels (cons (nth i channels) best-channels)))
		(reverse best-channels)))

;; INTERACT FUNCTION
;; The interact function is called by RUN-INTERACTION (or an equivalent function)
;; This will create an instance of an interaction
;; Determine the interacting agents (speaker and hearer)
(defmethod interact ((experiment gg-experiment) interaction &key)
  (declare (ignore interaction))
  (let* ((speaker (speaker experiment))
         (hearer (hearer experiment))
         (world (world experiment))
         (topic (nth (random (length world)) world))
         (context (let ((world-copy (copy-tree (world experiment))))
         				(remove topic world-copy :test #'equal)))
         (saliency (saliency topic context))
         (use-channels (select-channels experiment saliency (get-configuration experiment :saliency-strategy))))
  	(multiple-value-bind (scaled-topic scaled-context) (context-scaling topic context)
    	(initialize-agent speaker :topic scaled-topic)
    	(initialize-agent hearer :topic scaled-topic)
    	(let ((speaker-form (give-form speaker scaled-topic scaled-context use-channels)))
    		(if (null speaker-form)
	    		(finish-interaction experiment :no-categorisation-speaker)
	    	 	(let ((hearer-meaning (give-meaning hearer speaker-form)))
		    		(if (null hearer-meaning)
		    			(let ((successfully-learned (learn-topic hearer speaker-form scaled-topic scaled-context use-channels)))
		    				(if successfully-learned
		    					(finish-interaction experiment :learned-new-form)
		    					(finish-interaction experiment :no-categorisation-hearer)))
		    			(if (equal hearer-meaning scaled-topic)
			    			(progn (setf (communicated-successfully speaker) t)
			    				   (setf (communicated-successfully hearer) t)
			    				   (finish-interaction experiment :correct))
			    			(finish-interaction experiment :not-correct)))))))))

;; FINISH INTERACTION
(defgeneric finish-interaction (experiment finish-state)
	(:documentation "finish-interaction"))

(defmethod finish-interaction ((experiment gg-experiment) (finish-state (eql :no-categorisation-speaker)))
	(let ((speaker (speaker experiment)))
		(loop for agent in (interacting-agents experiment)
	      	  do (update-agent-age agent))
		(update-categorizer-age speaker)
		(growth speaker)))

(defmethod finish-interaction ((experiment gg-experiment) (finish-state (eql :no-categorisation-hearer)))
	(let ((speaker (speaker experiment))
		  (hearer (hearer experiment)))
		(loop for agent in (interacting-agents experiment)
		  	  do (update-agent-age agent)
		  	  do (update-categorizer-age agent))
		(update-categorizer-use speaker)
		(growth hearer)))

(defmethod finish-interaction ((experiment gg-experiment) (finish-state (eql :learned-new-form)))
	(loop for agent in (interacting-agents experiment)
	  	  do (update-agent-age agent)
	  	  do (update-categorizer-age agent)
	  	  do (update-categorizer-use agent)))

(defmethod finish-interaction ((experiment gg-experiment) (finish-state (eql :not-correct)))
	(let ((speaker (speaker experiment)))
		(loop for agent in (interacting-agents experiment)
		  	  do (update-agent-age agent)
		  	  do (align-agent agent (get-configuration experiment :alignment-strategy)))
		(update-categorizer-age speaker)
		(update-categorizer-use speaker)))

(defmethod finish-interaction ((experiment gg-experiment) (finish-state (eql :correct)))
	(let ((speaker (speaker experiment)))
		(loop for agent in (interacting-agents experiment)
		  	  do (update-agent-age agent)
		  	  do (align-agent agent (get-configuration experiment :alignment-strategy)))
		(update-categorizer-age speaker)
		(update-categorizer-use speaker)
		(update-categorizer-success speaker)
		(pruning speaker (get-configuration experiment :pruning-strategy))))

(defmethod finish-interaction :after ((experiment gg-experiment) finish-state)
	(let ((interaction-number (interaction-number (current-interaction experiment)))
		  (agent-influx (get-configuration experiment :agent-influx))
		  (object-influx (get-configuration experiment :object-influx))
		  (agent-influx-rate (get-configuration experiment :agent-influx-rate))
		  (object-influx-rate (get-configuration experiment :object-influx-rate)))
		(case agent-influx
			(:new (when (integerp (/ interaction-number agent-influx-rate))
						(new-agent experiment)))
			(:replace (when (integerp (/ interaction-number agent-influx-rate))
							(replace-agent experiment)))
			(:none nil))
		(when object-influx
			(when (integerp (/ interaction-number object-influx-rate))
				(more-objects experiment)))))

;; ALIGNMENT
(defgeneric align-agent (agent strategy)
	(:documentation "Perform the given alignment method"))

(defmethod align-agent ((agent gg-agent) (strategy (eql :no-alignment)))
  	nil)

(defmethod align-agent ((agent gg-agent) (strategy (eql :imitation)))
  	(loop for competitor in (get-form-competitors agent (applied-triple agent))
          do (setf (memory agent) (remove competitor (memory agent) :test #'equal))))

(defmethod align-agent ((agent gg-agent) (strategy (eql :minimal)))
  	(when (communicated-successfully agent)
    	(loop for competitor in (get-form-competitors agent (applied-triple agent))
              do (setf (memory agent) (remove competitor (memory agent) :test #'equal)))))

(defmethod align-agent ((agent gg-agent) (strategy (eql :li-1)))
	(let ((inc (get-configuration (experiment agent) :li-inc))
		  (dec (get-configuration (experiment agent) :li-dec)))
		(if (communicated-successfully agent)
			(progn (inc-score (applied-triple agent) inc)
				   (loop for competitor in (get-form-competitors agent (applied-triple agent))
				   	     do (dec-score agent competitor dec)))
			(when (equal (discourse-role agent) 'speaker)
				(dec-score agent (applied-triple agent) dec)))))

(defmethod align-agent ((agent gg-agent) (strategy (eql :li-2)))
	(let ((inc (get-configuration (experiment agent) :li-inc))
		  (dec (get-configuration (experiment agent) :li-dec)))
  		(if (communicated-successfully agent)
      		(progn (setf (score (applied-triple agent)) 
      			         (+ (* (score (applied-triple agent)) 
						    (- 1 inc))
                         inc))
					(loop for competitor in (get-form-competitors agent (applied-triple agent))
	                      do (dec-score agent competitor (* (score competitor) (- 1 dec)))))
      		(when (equal (discourse-role agent) 'speaker)
        		(dec-score agent (applied-triple agent) (* (score (applied-triple agent)) (- 1 dec)))))))

(defmethod align-agent ((agent gg-agent) (strategy (eql :li-3)))
	(let ((inc (get-configuration (experiment agent) :li-inc))
		  (dec (get-configuration (experiment agent) :li-dec)))
	  (if (communicated-successfully agent)
	      (progn (setf (score (applied-triple agent)) (+ (* (score (applied-triple agent)) (- 1 inc)) inc))
	        	 (if (equal (discourse-role agent) 'speaker)
	          		 (loop for competitor in (get-form-competitors agent (applied-triple agent))
	                       do (dec-score agent competitor (* (score competitor) (- 1 dec))))
	          		 (loop for competitor in (get-meaning-competitors agent (applied-triple agent))
	                       do (dec-score agent competitor (* (score competitor) (- 1 dec))))))
	      (when (equal (discourse-role agent) 'speaker)
	        (dec-score agent (applied-triple agent) (* (score (applied-triple agent)) (- 1 dec)))))))

;; INTRODUCE A NEW AGENT
(defgeneric new-agent (experiment)
	(:documentation "Introduce a new agent"))

(defmethod new-agent ((experiment gg-experiment))
	(let* ((pop-size (get-configuration experiment :population-size))
		   (agent (make-instance 'gg-agent :id (+ pop-size 1)
		   	                               :experiment experiment)))
		(setf (population experiment) (cons agent (population experiment)))
		(set-configuration experiment :population-size (+ pop-size 1))))

(defgeneric replace-agent (experiment)
	(:documentation "Replaces the oldest agent"))

(defmethod replace-agent ((experiment gg-experiment))
	(setf (population experiment) (sort (population experiment) #'> :key (lambda (agent) (age agent))))
	(let* ((oldest (first (population experiment)))
		   (oldest-id (id oldest))
		   (new-agent (make-instance 'gg-agent :id oldest-id
		   	                                   :experiment experiment)))
		(setf (first (population experiment)) new-agent)))

;; MORE OBJECTS IN EXPERIMENT
(defgeneric more-objects (experiment)
	(:documentation "Increase the number of objects in the experiment"))

(defmethod more-objects ((experiment gg-experiment))
	(let* ((data (read-data-file))
		   (new-obj (nth (random (length data)) data)))
		(if (not (member new-obj (world experiment)))
			(setf (world experiment) (cons new-obj (world experiment)))
			(more-objects experiment))))






