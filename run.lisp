
(asdf:operate 'asdf:load-op "guessing")

(in-package :guessing)

(defun run-experiments (strategies 
                        &key
                          (population-size 3)
                          (world-size 3)

                          (agent-influx :none)
                          (agent-influx-rate 500)
                          (object-influx nil)
                          (object-influx-rate 750)

                          (channels (list 'X 'Y 'WIDTH 'HEIGHT 'GREY))

                          (saliency-strategy :most-salient)
                          (saliency-threshold 0)

                          (node-selection-strategy :depth)

                          (alignment-strategy :li-1)
                          (li-inc 0.1)
                          (li-dec 0.2)

                          (pruning-strategy :on)
                          (pruning-age-threshold 20)
                          (pruning-success-thresold 0.05)
                          (pruning-use-thresold 10)

                          (number-of-interactions 2000)
                          (number-of-series 20)
                          (monitors 
                           '("export-lexicon-size" 
                           	 "export-repertoire-size" 
                           	 "export-discriminative-success" 
                           	 "export-communicative-success"
                           	 "export-lexical-coherence"
                             "export-population-size"
                             "export-scene-size")))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
   :experiment-class 'gg-experiment
   :number-of-interactions number-of-interactions
   :number-of-series number-of-series
   :monitors monitors
   :shared-configuration `(
                 (:population-size . ,population-size)
                 (:world-size . ,world-size)
                 (:agent-influx . ,agent-influx)
                 (:agent-influx-rate . ,agent-influx-rate)
                 (:object-influx . ,object-influx)
                 (:object-influx-rate . ,object-influx-rate)
   						   (:channels . ,channels)
   						   (:saliency-strategy . ,saliency-strategy)
   						   (:node-selection-strategy . ,node-selection-strategy)
                 (:alignment-strategy . ,alignment-strategy)
   						   (:li-inc . ,li-inc)
   						   (:li-dec . ,li-dec)
                 (:pruning-strategy . ,pruning-strategy)
                 (:pruning-age-threshold . ,pruning-age-threshold)
                 (:pruning-use-thresold . ,pruning-use-thresold)
                 (:pruning-success-thresold . ,pruning-success-thresold))
   :configurations strategies
   :output-dir (babel-pathname :directory `("examples" "guessing" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names)
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
   :raw-file-paths (loop for measure-name in measure-names
                         collect `("examples" "guessing" "raw-data" ,experiment-name ,measure-name))
   :average-windows 100
   :plot-directory '("examples" "guessing" "graphs")
   :error-bars '(:percentile 10 90) 
   :error-bar-modes '(:filled))
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name)
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot 
   :raw-file-paths 
   (loop for experiment-name in experiment-names
      collect `("examples" "guessing" "raw-data" ,experiment-name ,measure-name))
   :average-windows 100
   :captions experiment-names                  
   :plot-directory '("examples" "guessing" "graphs")
   :error-bars '(:percentile 10 90) :error-bar-modes '(:filled))
  (format t "~%Graphs have been created"))


(run-experiments '(;; (name-of-experiment ((:key . :value)(:key . :value)...))

                   ;(small ((:population-size . 3) (:world-size . 3)))
                   ;(medium ((:population-size . 4) (:world-size . 4)))
                   ;(large ((:population-size . 5) (:world-size . 5)))
                   ;(large ((:population-size . 10) (:world-size . 10)))
                   ;(extreme ((:population-size . 20) (:world-size . 20)))

                   ; (most-salient ((:saliency-strategy . :most-salient)))
                   ; (threshold-0 ((:saliency-strategy . :above-threshold) (:saliency-threshold . 0)))
                   ; (threshold-0.1 ((:saliency-strategy . :above-threshold) (:saliency-threshold . 0.1)))
                   ; (threshold-0.2 ((:saliency-strategy . :above-threshold) (:saliency-threshold . 0.2)))

                   ; (depth ((:node-selection-strategy . :depth)))
                   ; (most-use ((:node-selection-strategy . :most-use)))
                   ; (most-success ((:node-selection-strategy . :most-success)))

                   ; (very-small-inc ((:li-inc . 0.025) (:li-dec . 0.05)))
                   ; (small-inc ((:li-inc . 0.05) (:li-dec . 0.1)))
                   ; (medium-inc ((:li-inc . 0.1) (:li-dec . 0.2)))
                   ; (large-inc ((:li-inc . 0.2) (:li-dec . 0.4)))
                   ; (very-large-inc ((:li-inc . 0.5) (:li-dec . 0.5)))

                   ; (new-agent-250 ((:agent-influx . :new) (:agent-influx-rate . 250)))
                   ; (new-agent-500 ((:agent-influx . :new) (:agent-influx-rate . 500)))
                   ; (new-agent-750 ((:agent-influx . :new) (:agent-influx-rate . 750)))
                   ;(new-agent-1000 ((:agent-influx . :new) (:agent-influx-rate . 1000)))

                   ;(replace-agent-100 ((:agent-influx . :replace) (:agent-influx-rate . 100)))
                   ; (replace-agent-250 ((:agent-influx . :replace) (:agent-influx-rate . 250)))
                   ; (replace-agent-500 ((:agent-influx . :replace) (:agent-influx-rate . 500)))

                   (new-object-250 ((:object-influx . t) (:object-influx-rate . 250)))
                   ; (new-object-500 ((:object-influx . t) (:object-influx-rate . 500)))
                   ; (new-object-750 ((:object-influx . t) (:object-influx-rate . 750)))

                   ; (no-alignment ((:alignment-strategy . :no-alignment)))
                   ; (imitation ((:alignment-strategy . :imitation)))
                   ; (minimal ((:alignment-strategy . :minimal)))
                   ; (lateral-inhibition-1 ((:alignment-strategy . :li-1)))
                   ; (lateral-inhibition-2 ((:alignment-strategy . :li-2)))
                   ; (lateral-inhibition-3 ((:alignment-strategy . :li-3)))
                   ))

(create-graph-for-single-strategy :experiment-name "new-object-250" 
                                  :measure-names '("lexicon-size" 
                                                   "repertoire-size"))

(create-graph-for-single-strategy :experiment-name "new-object-250" 
								  :measure-names '("discriminative-success"
								  	               "communicative-success"
                                   "lexical-coherence"))

; (create-graph-for-single-strategy :experiment-name "new-object-750" 
;                                   :measure-names '("population-size" 
;                                                    "scene-size"))

; (create-graph-comparing-strategies :experiment-names '(
;                                                        "very-large-inc"
;                                                        )
;                                    :measure-name "lexical-coherence")



