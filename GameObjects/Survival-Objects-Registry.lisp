; ================================================================
; COMPOUNDED GAME OBJECT / ITEM LOGIC DEFINITION SCRIPT
; ================================================================
; Module: Survival-Object-Registry
; Purpose: Define survival / narrative / combat game objects
; Language: ALN (internal Lisp-like format for CLI processing)
; ================================================================

(defpackage :game.registry
  (:use :cl))

(in-package :game.registry)

; ----------------------
; Utility Macros
; ----------------------
(defmacro define-game-object (id type &rest props)
  `(progn
     (format t "~%[REGISTER] ~A :: type=~A props=~A" ,id ,type ',props)
     (list :id ,id :type ,type :props ',props)))

; Object storage
(defparameter *game-objects* (make-hash-table :test 'equal))

(defun register-object (id type &rest props)
  (setf (gethash id *game-objects*)
        (define-game-object id type props)))

(defun get-object (id)
  (gethash id *game-objects*))

(defun object-list ()
  (loop for k being the hash-keys of *game-objects* collect k))

; ================================================================
; OBJECT INITIALIZATION TABLE (100 entries)
; ================================================================

(defun init-default-game-objects ()
  (clrhash *game-objects*)

  ;; CORE STRUCTURAL / PLAYER OBJECTS
  (register-object "player-character.object" :entity :customizable t :stats '(:health 100 :stamina 100))
  (register-object "npc-survivor.object" :entity :ally t :encounter :random)
  (register-object "shelter.object" :structure :rest t :safe-zone t)
  (register-object "vehicle.object" :structure :drivable t :requires-parts t)

  ;; INTERACTION / WORLD OBJECTS
  (register-object "locked-door.object" :structure :access :restricted :requires '(:key :skill :lockpick))
  (register-object "workbench.object" :structure :crafting t :upgrade-station t)
  (register-object "campfire.object" :structure :heal 5 :cook-food t :event-trigger t)
  (register-object "radio.object" :trigger :quest-source t :world-event t)
  (register-object "rumor-board.object" :trigger :event-propagation t)
  (register-object "corpse.object" :entity :lootable t :infection-risk t)

  ;; CONTAINERS & STORAGE
  (register-object "backpack.object" :container :expand-inventory 20)
  (register-object "locked-safe.object" :container :locked t :requires :combination)
  (register-object "broken-vending-machine.object" :container :random-loot t)

  ;; RESOURCES / SURVIVAL
  (register-object "canteen.object" :item :container :fluid '(:clean :brackish :contaminated))
  (register-object "water-source-node.object" :node :resource :types '(:clean :contaminated))
  (register-object "ration-dispenser.object" :structure :food-supply t)
  (register-object "protein-bar.object" :item :food t :stamina-boost 10)
  (register-object "canned-beans.object" :item :food t :chance-of-spoilage 10)

  ;; HEALTH & STATUS ITEMS
  (register-object "first-aid-kit.object" :item :restore-health 40)
  (register-object "bandage.object" :item :heal-small t :prevents-infection t)
  (register-object "antipsychotics.object" :item :sanity-effect :positive)
  (register-object "thermal-blanket.object" :item :temperature-control t)

  ;; TOOLS & CRAFTING
  (register-object "multi-tool.object" :item :repair t :unlock t)
  (register-object "lockpick-set.object" :item :unlock-minigame t)
  (register-object "fuel-canister.object" :item :resource :fuel t)
  (register-object "water-purification-tablet.object" :item :purify-water t)

  ;; WEAPONS
  (register-object "rusty-knife.object" :weapon :melee t :durability 25)
  (register-object "glass-shard.object" :weapon :melee t :improvised t)
  (register-object "spore-bomb.object" :weapon :throwable t :aoe-effect :toxic)
  (register-object "rat-trap.object" :weapon :trap t :food-source t)

  ;; LORE & QUEST OBJECTS
  (register-object "old-photograph.object" :item :lore-trigger t)
  (register-object "journal-page.object" :item :quest-progress t)
  (register-object "mystic-rune.object" :item :myth-event t :status-effect :random)
  (register-object "mysterious-amulet.object" :item :stat-buff :cursed)

  ;; UTILITY & INTERACTION
  (register-object "keycard-terminal.object" :structure :locked-access-control t)
  (register-object "disguise-mask.object" :item :npc-interaction-modifier t)
  (register-object "broken-drone.object" :item :quest-repair t)
  (register-object "empty-bottle.object" :item :container :throwable t)
  (register-object "worn-boots.object" :item :mobility-boost t :event-trigger :footstep-sound)

  ;; Add filler procedural survival objects (loop for scalability)
  (loop for i from 1 to 55 do
    (register-object (format nil "crafted-item-~A.object" i) :item :crafted t :durability (random 50)))

  *game-objects*)

; ================================================================
; END OF MODULE
; ================================================================
; ================================================================
; COMPOUNDED GAME OBJECT / ITEM LOGIC DEFINITION SCRIPT (EXTENDED)
; ================================================================
; Module: Survival-Object-Registry (Expanded Verbose Table)
; Purpose: Provide a full verbose stat table / hook system
; Language: ALN (internal Lisp-like format for CLI processing)
; ================================================================

(defpackage :game.registry.verbose
  (:use :cl :game.registry))

(in-package :game.registry.verbose)

; ----------------------------------------------------------------
; Hook Templates (define extensible system-object behaviors)
; ----------------------------------------------------------------

(defun durability-system (object)
  (let ((dur (getf (getf (getf object :props) :durability) 0)))
    (when (and dur (> dur 0))
      (decf (getf (getf object :props) :durability) 1))))

(defun spoilage-system (object)
  (let ((spoil (getf (getf object :props) :chance-of-spoilage)))
    (when spoil
      (if (< (random 100) spoil)
          (setf (getf object :props) :spoiled t)))))

(defun contamination-system (object water-type)
  (when (and (eq (getf object :id) "canteen.object")
             (member water-type '(:brackish :contaminated)))
    (setf (getf object :props) :contamination-level water-type)))

(defun sanity-system (character object)
  (let ((effect (getf (getf object :props) :sanity-effect)))
    (cond
      ((eq effect :positive) (incf (getf (getf character :props) :sanity) 10))
      ((eq effect :negative) (decf (getf (getf character :props) :sanity) 10)))))

; ----------------------------------------------------------------
; Extended Registration: Verbose Properties for Selected Objects
; ----------------------------------------------------------------

(defun register-verbose-objects ()
  ;; Weapons
  (register-object "rusty-knife.object" :weapon
                   :melee t :durability 25
                   :damage 8 :crit-chance 5 :speed 4)
  (register-object "spore-bomb.object" :weapon
                   :throwable t :aoe-effect :toxic
                   :damage 25 :status-effect :poison :radius 3)

  ;; Healing Items
  (register-object "first-aid-kit.object" :item
                   :restore-health 40 :uses 3 :weight 2)
  (register-object "bandage.object" :item
                   :heal-small t :prevents-infection t
                   :uses 5 :weight 0.2)

  ;; Food + Spoilage
  (register-object "canned-beans.object" :item
                   :food t :heal 10 :stamina-regen 15
                   :chance-of-spoilage 10 :weight 1.5)
  (register-object "protein-bar.object" :item
                   :food t :stamina-boost 10 :heal 5
                   :shelf-life :long :weight 0.3)

  ;; Fluids / Containers
  (register-object "canteen.object" :item
                   :container :fluid '(:clean :brackish :contaminated)
                   :capacity 1.0 :durability 30)

  ;; Narrative & Sanity Items
  (register-object "old-photograph.object" :item
                   :lore-trigger t :sanity-effect :positive)
  (register-object "mystic-rune.object" :item
                   :myth-event t :status-effect :random :sanity-effect :negative)

  ;; Armor / Wearables
  (register-object "worn-boots.object" :item
                   :mobility-boost t :defense 2
                   :durability 35 :event-trigger :footstep-sound))

; ==================================================================
; INIT FULL VERBOSE TABLE
; ==================================================================
(defun init-verbose-object-table ()
  (init-default-game-objects)
  (register-verbose-objects)
  (format t "~%[VERBOSE-TABLE] Extended Game Object Registry Loaded.")
  (format t "~%[INFO] Objects now include durability, damage, spoilage, psych-effects, and crafting hooks.")
  *game-objects*)
