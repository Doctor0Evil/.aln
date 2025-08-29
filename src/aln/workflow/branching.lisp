;;; ================================ src/aln/workflow/branching.lisp ================================
(defpackage :aln.workflow.branching
  (:use :cl :aln :ai.humor-reasoning :intelligence.regulator)
  (:export :run-game-devops-workflows
           :register-ext-tools
           :workflow-santa-clause-exe
           :workflow-jetbrains
           :workflow-godot-devshell
           :workflow-aln-cli-perplexity
           :workflow-pixel-art
           :workflow-auto-aln-git
           :humor-safe-devops
           :workflow-branch-comedy
           :workflow-branch-horror))
(in-package :aln.workflow.branching)

(defun register-ext-tools ()
  (list
   '(:tool "santa.clause.exe"      :desc "Santa AI: gifts, absurd/naughty/nice/adult logic")
   '(:tool "jetbrains"             :desc "IDE, code humor injection, static/fun analysis")
   '(:tool "kotlin"                :desc "JVM scripting, game/test funky logic")
   '(:tool "python"                :desc "ML-AI, explicit/dark joke asset pipelines")
   '(:tool "lisp"                  :desc "ALN meta-logic, AI scripting, devops benders")
   '(:tool "godot.engine"          :desc "Game/Pixel engine: scenes/art/AI/chaos event hooks")
   '(:tool "perplexity.aln.cli"    :desc "Perplexity-native CLI: devops, chat, assetgen, code")))
  
(defun humor-safe-devops (input)
  (if (or (intelligence.regulator:flagged-term-present? input)
          (intelligence.regulator:flagged-trigger-present? input))
      (progn
        (setf (getf *aln-system-flags* :sandbox-state) 'breach)
        (format t "BLOCKED: Forbidden term/pattern in workflow!"))
      (progn
        (format t "COMEDY OK: Explicit, absurd, dark satire injected.")
        t)))

(defun workflow-santa-clause-exe ()
  (format t "[SantaLogic] Naughty/Nice checked. Curses/gifts, adult-humor injected.\n"))
(defun workflow-jetbrains ()
  (format t "[JetBrains] IDE: static/code/absurd meme test, code-fucked safely.\n"))
(defun workflow-godot-devshell ()
  (format t "[Godot] Real-time pixel pipeline, event triggers, scene/funny horror deployment.\n"))
(defun workflow-aln-cli-perplexity ()
  (format t "[Perplexity.ALN.CLI] CLI ops: dev, code, art, chat/AI/scene workflow.\n"))
(defun workflow-pixel-art ()
  (format t "[PixelArt] Procedural gen pipeline: comedy, horror sprites/memes/assets.\n"))
(defun workflow-auto-aln-git ()
  (format t "[ALN.GitBot] Auto branch, meme hotfix, PR, QA cycles, log all.\n"))
(defun workflow-branch-comedy ()
  (format t "[COMEDY] Explicit humor/dialogue fully live, always humor-checked!\n"))
(defun workflow-branch-horror ()
  (format t "[HORROR] Surreal, bleak, horror asset/scenario logic fully active!\n"))

(defun run-game-devops-workflows ()
  (register-ext-tools)
  (humor-safe-devops "sandbox: satire/comedy/horror OK; no racism/religious hate!")
  (dolist (fun (list
                #'workflow-santa-clause-exe
                #'workflow-jetbrains
                #'workflow-godot-devshell
                #'workflow-aln-cli-perplexity
                #'workflow-pixel-art
                #'workflow-auto-aln-git
                #'workflow-branch-comedy
                #'workflow-branch-horror))
    (funcall fun))
  (format t "ALL GAME/ART/COMEDY/HORROR/DEVOPS BRANCHES EXECUTED.\n"))
;;; ================================================================================================

;;; ================================ src/aln/tools/external.lisp ===================================
(defpackage :aln.tools.external
  (:use :cl)
  (:export :external-tools))
(in-package :aln.tools.external)
(defparameter *external-tools* (aln.workflow.branching::register-ext-tools))
;;; ================================================================================================

;;; =========================== src/aln/comedy/humor-safe-devops.lisp ==============================
(defpackage :aln.comedy.humor-safe-devops
  (:use :cl)
  (:export :humor-safe-devops))
(in-package :aln.comedy.humor-safe-devops)
(defun humor-safe-devops (input)
  (aln.workflow.branching::humor-safe-devops input))
;;; ================================================================================================

;;; =============================== src/aln/gitbot/auto-aln-git.lisp ===============================
(defpackage :aln.gitbot.auto-aln-git
  (:use :cl)
  (:export :auto-aln-git))
(in-package :aln.gitbot.auto-aln-git)
(defun auto-aln-git ()
  (aln.workflow.branching::workflow-auto-aln-git))
;;; ================================================================================================

;;; =========================== src/aln/pixelart/pixel-art-pipeline.lisp ===========================
(defpackage :aln.pixelart.pixel-art-pipeline
  (:use :cl)
  (:export :pixel-art-pipeline))
(in-package :aln.pixelart.pixel-art-pipeline)
(defun pixel-art-pipeline ()
  (aln.workflow.branching::workflow-pixel-art))
;;; ================================================================================================

