;; File: src/nightshift/how-to-be-funny-ai.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun humor/factor (topic)
  "Processes humor factor based on macabre context"
  (case topic
    (:workplace '("graveyard shift" "skeleton crew" "OSHA haunting"))
    (:food '("eat your coworkers" "dinner break of souls"))
    (:irony '("direct deposit in souls" "cash = coffins"))
    (:knockknock '("Nobody. Nobody has flesh left."))
    (t '("dark laughter echoes..."))))

(defun humor/generate-line (topic)
  "Returns one formatted banter line"
  (let* ((setup (first (humor/factor topic)))
         (twist (second (humor/factor topic))))
    (cond
      ((and setup twist) (format nil "~A... oh, and ~A." setup twist))
      (setup (format nil "~A." setup))
      (t "[silence, then screaming]"))))

(defun how.to.be.funny.ai.exe (n)
  "Generates N macabre banter lines as dark humor"
  (loop for i from 1 to n
        for topic in '(:workplace :food :irony :knockknock)
        do (format t "[NightShift>>] ~A~%" (humor/generate-line topic))))

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
