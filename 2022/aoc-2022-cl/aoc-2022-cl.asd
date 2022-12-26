(defsystem "aoc-2022-cl"
           :version "0.1.0"
           :author ""
           :license ""
           :depends-on ("str" "cl-ppcre" "trivia" "serapeum" "arrows")
           :components ((:module "src"
                                 :components
                                 ((:file "01")
                                  (:file "02a")
                                  (:file "02b")
                                  (:file "03a")
                                  (:file "03b")
                                  (:file "04a")
                                  (:file "04b"))))
           :description "")
