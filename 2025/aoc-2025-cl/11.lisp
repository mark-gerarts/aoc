(in-package :aoc-2025-cl)

(defun to-keyword (label)
  (intern (string-upcase label) :keyword))

(defun parse-line (line)
  (let* ((ids (sr:split-sequence #\Space (remove #\: line)))
         (src (to-keyword (first ids)))
         (outputs (mapcar #'to-keyword (rest ids))))
    (cons src outputs)))

(defparameter *device-map*
  (sr:~>> "input/11.test"
          uiop:read-file-lines
          (remove-if #'sr:blankp)
          (mapcar #'parse-line)
          (ax:alist-hash-table)))

(defun subgraph (graph start)
  "Given a graph and a start node, returns a graph of all nodes reachable from
start."
  (loop with new-graph = (sr:dict)
        with frontier = (list start)
        while frontier
        for next-item = (pop frontier)
        for neighbors = (sr:@ graph next-item)
        unless (or (sr:@ new-graph next-item) (not neighbors))
          do (setf (sr:@ new-graph next-item) neighbors)
          and do (setf frontier (append frontier neighbors))
        finally (return new-graph)))

(defun indegree (graph node)
  "Calculate the indegree of a node (= number of incoming edges)"
  (loop for neighbors being the hash-values of graph
        when (member node neighbors)
          count it))

(format t "TODO: finish implementation! ~%")
