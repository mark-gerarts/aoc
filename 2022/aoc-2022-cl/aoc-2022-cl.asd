(defsystem "aoc-2022-cl"
           :version "0.1.0"
           :author ""
           :license ""
           :depends-on ("str" "cl-ppcre" "trivia" "serapeum")
           :components ((:module "src"
                                 :components
                                 ((:file "01")
                                  (:file "02a")
                                  (:file "02b"))))
           :description "")
