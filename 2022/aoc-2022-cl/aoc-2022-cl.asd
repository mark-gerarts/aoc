(defsystem "aoc-2022-cl"
           :version "0.1.0"
           :author ""
           :license ""
           :depends-on ("str"
                        "cl-ppcre"
                        "trivia"
                        "trivia.ppcre"
                        "serapeum"
                        "arrows")
           :components ((:module "src"
                                 :components
                                 ((:file "01")
                                  (:file "02")
                                  (:file "03a")
                                  (:file "03b")
                                  (:file "04a")
                                  (:file "04b")
                                  (:file "05a")
                                  (:file "06")
                                  (:file "07")
                                  (:file "08")
                                  (:file "09")
                                  (:file "10")
                                  (:file "11"))))
           :description "")
