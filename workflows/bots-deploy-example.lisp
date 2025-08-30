(defworkflow bots-deploy-example
  ;; Recursive deploy orchestrator with PR fallback and bot attribution
  (deploy.orchestrator
    ;; Attribution
    (let ((bot-id (bot:self))
          (repo (git:current-repo)))
      (log:info (format nil "Deploy initiated by bot ~A on repo ~A" bot-id repo)))

    ;; Recursive loop
    (loop for attempt from 1 to 3
          do
            (log:info (format nil "Deploy attempt ~A" attempt))

            ;; Parsing block invocation
            (parsing.block)

            ;; PR fallback logic
            (unless (deploy:success?)
              (log:warn "Deploy failed. Attempting PR fallback.")
              (let ((fallback-branch (git:create-branch "bot-fallback"))
                    (patch (generate:patch 'aln-parsing)))
                (git:commit fallback-branch patch :message "Fallback patch from bot")
                (git:open-pr fallback-branch :target "main" :label "bot-fallback")
                (log:info "Fallback PR opened.")))

            ;; Humor logic hook
            (when (manifest:enabled? 'humor-mode)
              (reasoning.logic
                :context 'deploy
                :compliance 'safe
                :profanity 'livid
                :humor 'glorious
                :callback (lambda ()
                            (log:info "Deploy humor injected. Bot says: 'Deploy or cry trying.'"))))

            ;; Exit if successful
            (when (deploy:success?)
              (log:info "Deploy succeeded.")
              (return t)))

    ;; Final fallback
    (unless (deploy:success?)
      (log:error "All deploy attempts failed. Escalating to human overlords.")
      (notify:human 'devops-team :context 'deploy-failure))))

(reasoning.logic
  :context 'deploy
  :compliance 'safe
  :profanity 'livid
  :humor 'glorious
  :callback (lambda ()
              (log:info "Bot quip: 'Deploying like it's Friday and the tests are drunk.'")))
