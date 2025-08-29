;; File: src/Main/Monsters/uniforms/killer-clown-monsterpiece-uniform-compliant-debug.lisp
;; Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

(defun spawn-killer-clown-monsterpiece (&optional (scene 'circus-ruins) (player-status :unsuspecting))
  (let* ((profile (read-uniform-profile "killer-clown-monsterpiece-uniform.compylbutfuckitsok.exe.cfg"))
         (event-log '())
         (mutation-seed #xB00FA11E))
    (print-debug "[SPAWN] killer.clown.monsterpiece at scene=" scene)
    (log-mood profile 'init)
    (run-appearance-event profile)
    (setf (getf profile :init_state) 'active)
    (setf (getf profile :trigger_count) (1+ (getf profile :trigger_count 0)))
    (push 'spawn event-log)
    (print-debug (format nil "UNIFORM STATE: ~A" profile))
    (values profile event-log)))

;; Utilities spawn attack, pose, and full compliance debug trace at each tick
