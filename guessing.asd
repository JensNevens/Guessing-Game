
(in-package #:asdf)

(defsystem "guessing"
  :depends-on (:experiment-framework 
               :utils
               :monitors
               :plot-raw-data
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
   ((:file "categorizer")
    (:file "agent")
    (:file "monitors")
    (:file "guessing-game")))
