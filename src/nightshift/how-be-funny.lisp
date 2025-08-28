;; File: src/nightshift/how-be-funny.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun how.be.funny.exe (context)
  "Dark humor generator: horrorcore flavor"
  (let* ((mood '(:dark :grotesque :deadpan))
         (filters '(no-copyright reproduce yes-roleplay safe-dark-humor))
         (rules '((funny . (juxt (violence->absurd) (pain->punchline)))
                  (tone . deadpan)
                  (scene . nightshift))))
    (format t ">> Welcome to the Night Shift ~%")
    (cond ((member :dark mood)
           (format t ">> Here screams & laughter mix, same frequency. ~%"))
          ((member :grotesque mood)
           (format t ">> Reminder: mop the morgue before sunrise. ~%")))
    (apply #'list rules)))
