;; ===========================
;; 1. Skill, Subskill, and Party Structures
;; ===========================

(defstruct skill
  id
  name
  index
  description
  subskills
  unlocks
  advancement-menu
  synergy-links)

(defstruct subskill
  id
  name
  index
  description
  unlocks
  advancement-menu)

;; Character has a skill-table (id -> level), and an attribute-table (id -> value)
(defstruct character
  name
  skills      ;; Alist (id . level)
  attributes  ;; Alist (id . value)
  party       ;; List of other characters (followers/players/AI)
  specializations ;; Alist (skill-id . specialization-keyword)
)

;; Party for easy group ops:
(defstruct party
  leader
  members    ;; List of character objects
)

;; ===========================
;; 2. Skill Catalog Definition
;; ===========================

(defparameter *skill-catalog*
  (list
   (make-skill
    :id 'survival :name "Survival" :index 1
    :description "Endure the haunted wild: hunger, thirst, sickness, and exposure."
    :subskills (list
      (make-subskill :id 'foraging :name "Foraging" :index 101
        :description "Find/identify edible items, reduce food risk."
        :unlocks '(ident-plant night-harvest))
      (make-subskill :id 'scavenging :name "Scavenging" :index 102
        :description "Search ruins/hidden caches, unlock rare loot."
        :unlocks '(scavenge-cache salvage-relic)))
    :unlocks '(resist-starve sense-weather)
    :advancement-menu '(("Focus: Forager" foraging) ("Focus: Scavenger" scavenging))
    :synergy-links '(prayer stealth))

   (make-skill
    :id 'prayer :name "Prayer" :index 2
    :description "Channel mystery to repel evil, protect, unlock hope and lost power."
    :subskills (list
      (make-subskill :id 'calm :name "Calm" :index 201
        :description "Soothe fear for self/allies, reduce supernatural panic."
        :unlocks '(soothe-aura stillness-charm))
      (make-subskill :id 'banishing :name "Banishing" :index 202
        :description "Pacify/dispel horrors, at risk of backlash."
        :unlocks '(light-banishing silence-hollows)))
    :unlocks '(blessed-zone group-aura)
    :advancement-menu '(("Advance: Calm Aura" calm) ("Advance: Banishing" banishing))
    :synergy-links '(charisma luck))

   (make-skill
    :id 'combat :name "Combat" :index 3
    :description "Weapons, resilience, reflex—stand your ground and strike back."
    :subskills (list
      (make-subskill :id 'melee :name "Melee" :index 301 :description "Mastery of blades, blunt force, power." :unlocks '(cleave disarm))
      (make-subskill :id 'ranged :name "Ranged" :index 302 :description "Projectile precision, quick draws." :unlocks '(quickshot focus-fire))
      (make-subskill :id 'unarmed :name "Unarmed" :index 303 :description "Grapple, defend, counter with bare hands." :unlocks '(counter-grab nerve-strike)))
    :unlocks '(adrenaline recover-faster)
    :advancement-menu '(("Improve: Melee" melee) ("Improve: Ranged" ranged) ("Improve: Unarmed" unarmed))
    :synergy-links '(stealth survival))

   (make-skill
    :id 'stealth :name "Stealth" :index 4
    :description "Avoid, mislead, and stalk; cloak your presence, outwait doom."
    :subskills (list
      (make-subskill :id 'camouflage :name "Camouflage" :index 401
        :description "Master hiding in any horror or weather."
        :unlocks '(evasion-blend shadow-rest))
      (make-subskill :id 'silent-move :name "Silent Move" :index 402
        :description "Move nearly without noise, slip through tension."
        :unlocks '(fluid-step mask-approach)))
    :unlocks '(hide-fear silent-drop)
    :advancement-menu '(("Learn: Camouflage" camouflage) ("Learn: Silent Move" silent-move))
    :synergy-links '(luck mechanics))

   (make-skill
    :id 'mechanics :name "Mechanics" :index 5
    :description "Repair, invent, adapt—tools, traps, locks, old machines."
    :subskills (list
      (make-subskill :id 'craft :name "Primitive Craft" :index 501
        :description "Quickly jury-rig survival gear and repairs."
        :unlocks '(patch-wearer fast-repair))
      (make-subskill :id 'advanced-craft :name "Advanced Crafting" :index 502
        :description "Complex solutions—traps, hacks, fusion."
        :unlocks '(tripwire webtrap hack-circuit)))
    :unlocks '(trap-find resist-decay)
    :advancement-menu '(("Focus: Primitive Craft" craft) ("Focus: Advanced Engineering" advanced-craft))
    :synergy-links '(survival stealth))

   (make-skill
    :id 'charisma :name "Charisma" :index 6
    :description "Influence others—lead, manipulate, comfort; forge alliances."
    :subskills (list
      (make-subskill :id 'speech :name "Speech" :index 601
        :description "Dialogue edge, unlock secrets, fast-talk."
        :unlocks '(convince-secret calm-hostile))
      (make-subskill :id 'inspire :name "Inspire" :index 602
        :description "Bolster party courage, sharpen unity."
        :unlocks '(rally resolve-up)))
    :unlocks '(recruit persuade)
    :advancement-menu '(("Specialize: Speech" speech) ("Specialize: Inspire" inspire))
    :synergy-links '(prayer survival))

   ;; LUCK: Defined as an attribute, not a skill
   ))

;; ===========================
;; 3. Luck as Universal Attribute (Not a Skill)
;; ===========================

(defstruct attribute
  id name base-value description influences)     ;; influences: list of skills/event rolls impacted

(defparameter *attributes*
  (list
   (make-attribute :id 'luck :name "Luck" :base-value 5
       :description "Sheer fate: governs probability, rare events, roll outcomes. Synergizes with all skills, rerolls, oddities."
       :influences '(survival prayer combat stealth charisma mechanics))))

;; ===========================
;; 4. Skill Menu/Advancement UI (with Party/Follower/Coop Integration)
;; ===========================

(defun get-skill-level (char skill-id)
  (or (cdr (assoc skill-id (character-skills char))) 1))

(defun show-skill-menu (char)
  (format t "~%==== SKILL TREE MENU for ~A ====" (character-name char))
  (dolist (skill *skill-catalog*)
    (let ((lvl (get-skill-level char (skill-id skill))))
      (format t "~%[~A] ~A (Lv~D): ~A~%" (skill-index skill) (skill-name skill) lvl (skill-description skill))
      ;; Subskills
      (dolist (sub (skill-subskills skill))
        (let ((slvl (get-skill-level char (subskill-id sub))))
          (format t "   - ~A (Lv~D): ~A~%" (subskill-name sub) slvl (subskill-description sub))))
      ;; Unlocks and specializations
      (format t "   Options: ~{~A~^ | ~}~%" (mapcar #'first (skill-advancement-menu skill)))
      (format t "   Synergy: ~{~A~^, ~}~%" (mapcar (lambda (sid) (skill-name (find sid *skill-catalog* :key #'skill-id))) (skill-synergy-links skill))))))

;; Party effect propagation (AOE/Co-op: no tether/lag ever)
(defun party-propagate-skill (leader skill-id effect)
  (let* ((party (character-party leader))
         (radius (10 + (* 2 (get-skill-level leader skill-id)))))
    (dolist (member (party-members party))
      (when (<= (distance leader member) radius)
        (apply-effect member effect)))))

;; Follower-specific (AI/ally): always benefit from leader auras when within range, even on different maps/regions; works with networked free-travel

(defun update-follower-buffs (leader followers skill-id)
  (let ((power (get-skill-level leader skill-id)))
    (dolist (f followers)
      (when (<= (distance leader f) (+ 6 power))
        (grant-buff f (skill-name (find skill-id *skill-catalog* :key #'skill-id))) )))

;; ===========================
;; 5. Advancement/Milestone/Unlock Example
;; ===========================

(defun advance-skill (char skill-id option)
  (let ((skill (find skill-id *skill-catalog* :key #'skill-id)))
    (if (assoc option (skill-advancement-menu skill))
        (progn
          (setf (cdr (assoc skill-id (character-specializations char))) option)
          (format t "[Advancement] ~A chose specialization ~A.~%" (character-name char) option))
        (format t "[Error] Invalid specialization for ~A.~%" (skill-name skill)))))

;; ===========================
;; 6. Milestone Example (Automatic Cake!)
;; ===========================

(defun check-milestones (char)
  (dolist (skill *skill-catalog*)
    (let ((lvl (get-skill-level char (skill-id skill))))
      (cond
        ((and (eql skill 'combat) (>= lvl 25)) (grant-title char "Blooded Survivor"))
        ((and (eql skill 'prayer) (>= lvl 30)) (grant-title char "Aura of Stillness"))))))

;; ===========================
;; 7. Synergy Menu Explorer
;; ===========================

(defun show-synergies (skill-id)
  (let* ((skill (find skill-id *skill-catalog* :key #'skill-id)))
    (format t "~%Skill: ~A (Synergizes with: ~{~A~^, ~})~%"
            (skill-name skill)
            (mapcar (lambda (sid) (skill-name (find sid *skill-catalog* :key #'skill-id))) (skill-synergy-links skill)))))

