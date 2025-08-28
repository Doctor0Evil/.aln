;; File: src/nightshift/how-to-be-funny-dynamic-ai.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun dark-overlay (phrase)
  "Overlays macabre twist to a phrase dynamically"
  (let ((twists '("with corpses applauding"
                  "as the morgue clock strikes midnight"
                  "while skeletons take notes"
                  "fed through a meat grinder of souls"
                  "watched by eyeless faces in the dark")))
    (format nil "~A ~A" phrase (nth (random (length twists)) twists))))

(defun line-transform (input)
  "Transforms an input noun/phrase into dark humor"
  (cond
    ;; workplace metaphors
    ((search "work" input)
     (dark-overlay "Welcome to the graveyard shift"))
    ;; food related
    ((search "food" input)
     (dark-overlay "Dinner break means cannibal time"))
    ;; family / love inversion
    ((or (search "love" input) (search "family" input))
     (dark-overlay "Kisses taste like formaldehyde here"))
    ;; random boring nouns
    ((search "coffee" input)
     (dark-overlay "Your coffee is brewed with ashes"))
    ;; BACKUP BRANCH - if no matches
    (t (backup-transform input))))

(defun backup-transform (input)
  "Backup logic if no standard transformation finds a path"
  (let ((fallbacks '("The silence cracks like a ribcage..."
                     "Nobody laughs anymore—lacking lungs."
                     "This joke was buried, yet still speaks."
                     "The corpse tells it better anyway.")))
    (nth (random (length fallbacks)) fallbacks)))

(defun how.to.be.funny.ai.exe (user-inputs)
  "Generates macabre jokes dynamically from a list of user inputs"
  (dolist (input user-inputs)
    (let ((joke (line-transform (string-downcase input))))
      (format t "[NightShift/Joke>>] ~A~%" joke))))
