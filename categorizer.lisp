(defpackage :guessing
  (:use :common-lisp :utils :monitors :web-interface :plot-raw-data
        :experiment-framework))

(in-package :guessing)

(defclass cat-node ()
	((id
		:accessor id :initform (gensym "node-")
		:documentation "Unique id for each node")
	(parent
		:accessor parent :initarg :parent :initform nil :type (or nil 'cat-node)
		:documentation "Parent of current node")
	(left
		:accessor left :initarg :left :initform nil :type (or nil 'cat-node)
		:documentation "Left child of current node")
	(right
		:accessor right :initarg :right :initform nil :type (or nil 'cat-node)
		:documentation "Right child of current node")
	(channel-name
		:accessor channel-name :initarg :channel-name :initform nil :type (or nil symbol)
		:documentation "channel-name of this node")
	(min-val
		:accessor min-val :initarg :min-val :initform 0 :type number
		:documentation "Min value of this node")
	(max-val
		:accessor max-val :initarg :max-val :initform 1 :type number
		:documentation "Max value of this node")
	(depth
		:accessor depth :initarg :depth :initform 0 :type number
		:documentation "Depth of a node in the tree")
	(age
		:accessor age :initarg :age :initform 0 :type number
		:documentation "Age of this categorizer")
	(use
		:accessor use :initarg :use :initform 0 :type number
		:documentation "Use of this categorizer")
	(success
		:accessor success :initarg :success :initform 0 :type number
		:documentation "Success of this categorizer"))
	(:documentation "categorizer node"))

;; UPDATE TREE AGE
(defgeneric tree-age (node)
	(:documentation "Increment age of all nodes in the tree"))

(defmethod tree-age ((node cat-node))
	(progn
		(incf (age node))
		(when (and (not (null (left node))) 
			       (not (null (right node))))
			(progn
				(tree-age (left node))
				(tree-age (right node))))))

;; GROW TREE
(defgeneric grow (node)
	(:documentation "Descent down the tree and grow a new node"))

(defmethod grow ((node cat-node))
	(if (and (null (left node))
		     (null (right node)))
	    (let* ((channel-name (channel-name node))
	    	   (min (min-val node))
	    	   (max (max-val node))
	    	   (half (abs (float (/ (+ max min) 2))))
	    	   (left-node (make-instance 'cat-node :parent node
	    	   	                                   :channel-name channel-name
	    	   	                                   :min-val min
	    	   	                                   :max-val half
	    	   	                                   :depth (+ (depth node) 1)))
	    	   (right-node (make-instance 'cat-node :parent node
	    	   	                                    :channel-name channel-name
	    	   	                                    :min-val half
	    	   	                                    :max-val max
	    	   	                                    :depth (+ (depth node) 1))))
	    	(setf (left node) left-node)
	    	(setf (right node) right-node)
	    	node)
		(let ((direction (random 2)))
			(case direction
				(0 (grow (left node)))
				(1 (grow (right node)))))))

;; PRUNE TREE
(defgeneric prune (node age use success)
	(:documentation "Prune starting from the current node"))

(defmethod prune ((node cat-node) (pruning-age number) (pruning-use number) (pruning-suc float))
    (cond
        ((and (null (left node)) (null (right node)))
            nil)
        ((and (can-prune (left node) pruning-age pruning-use pruning-suc) 
        	  (can-prune (right node) pruning-age pruning-use pruning-suc))
            (progn (setf (left node) nil)
                   (setf (right node) nil)))
        (t
            (progn (prune (left node) pruning-age pruning-use pruning-suc)
                   (prune (right node) pruning-age pruning-use pruning-suc)))))

(defun can-prune (node pruning-age pruning-use pruning-suc)
    (let ((age (age node))
          (use (use node))
          (success (success node)))
        (if (not (= use 0))
            (if (and (not (null (left node))) (not (null (right node))))
                (and (> age pruning-age)
                	 (> use pruning-use)
                     (< (float (/ success use)) pruning-suc)
                     (can-prune (left node) pruning-age pruning-use pruning-suc)
                     (can-prune (right node) pruning-age pruning-use pruning-suc))
                (and (> age pruning-age)
                	 (> use pruning-use)
                     (< (float (/ success use)) pruning-suc)))
            (if (and (not (null (left node))) (not (null (right node))))
                (and (> age pruning-age)
                     (can-prune (left node) pruning-age pruning-use pruning-suc)
                     (can-prune (right node) pruning-age pruning-use pruning-suc))
                (> age pruning-age)))))
	



