;; File: repo-logic/hierarchical-scan.lisp
;; Github: https://github.com/Doctor0Evil/ALN_Programming_Language/blob/main/repo-logic/hierarchical-scan.lisp

(defun aln-hierarchical-scan (&key (root-path "./") (reposym 'ALN_Programming_Language))
  "Recursively scans ALN repo directory from ROOT-PATH,
   printing an indented tree structure for hierarchy overview."
  (labels ((scan (dir level)
            (dolist (entry (directory (merge-pathnames "*" dir)))
              (when (not (member (pathname-name entry) '("." "..")))
                (let ((indent (make-string (* 2 level) :initial-element #\Space)))
                  (format t "~A~A~%" indent (file-namestring entry))
                  (when (probe-file entry)
                    (if (and (not (file-directory-p entry)) (not (pathname-type entry)))
                        nil
                        (scan entry (1+ level))))))))
    (format t "~&ALN Repo Hierarchy:~%")
    (scan root-path 0)))
