;;; ====================================================
;;; NPC CHILD BRANCH LOGIC — Real-time, reactive AI behavior
;;; ====================================================
(defun npc-react (npc input event)
  (case event
    (:torchlight-phoenix
      (if (< (random 1.0) 0.5)
          (npc-speak npc "I saw it too… reborn flame.")
          (npc-emote npc 'fear)))
    (:echoed-whispers
      (npc-speak npc "Shadows repeat your words, but wrong…"))
    (:forest-mimicry
      (npc-speak npc "The woods mock us, copying our moves…"))
    (:shadow-curtain
      (npc-gesture npc 'curtain-drop))
    (:lucky-trinket
      (npc-pickup npc "strange humming trinket"))
    (t (npc-gesture npc 'confused))))
