;; File: workflows/bots-deploy-example.lisp
;; Purpose: Declarative/compositional logic mirroring the ALN Command Terminal's process flow
;; Features: Implements deployment, repo hygiene, docker, event replay, logging, and ML-driven auto-repair for bots.

(ALN-DEFUN BOTS-DEPLOY-EXAMPLE
  (:INPUTS ((ENVIRONMENT :REQUIRED T :DEFAULT "staging")))
  (:PERMISSIONS (CONTENTS WRITE) (PACKAGES WRITE) (ID-TOKEN WRITE) (WORKFLOWS WRITE))
  (:JOBS
    (BUILD-AND-PUSH
      (:NAME "Build & Push github.aln.bots")
      (:RUNS-ON (UBUNTU-LATEST))
      (:ENVIRONMENT ENVIRONMENT)
      (:DEFAULTS (:WORKING-DIRECTORY "bots"))
      (:STEPS
        (CHECKOUT-SOURCE (:ACTION 'GITHUB-CHECKOUT :FETCH-DEPTH 0))
        (USE-NODE18 (:ACTION 'NODE-SETUP :NODE-VERSION 18 :CACHE 'NPM
                    :CACHE-DEPENDENCY-PATH "bots/package-lock.json"))
        (INSTALL-DEPS (:ACTION 'RUN :SCRIPT "npm ci"))
        (LINT-MANIFEST
          (:ACTION 'RUN
           :SCRIPT "npm run lint --if-present || true"
           :SCRIPT "node -e \"JSON.parse(require('fs').readFileSync('config/bots.manifest.json','utf8')); console.log('manifest: OK')\""))
        (ML-REPAIR-PROFANITY
          (:ACTION 'RUN
           :SCRIPT "git ls-files '*.yml' '*.yaml' '*.aln' | while read file; do
                      echo \"Repairing $file\"
                      node scripts/aln-repair-operator.js \"$file\" --adapt=fun.adapt=evolve.real.aln
                    done
                    if ! git diff --quiet; then
                      git config user.name \"aln-bot\"
                      git config user.email \"aln-bot@users.noreply.github.com\"
                      git commit -am \"chore: background repairs (.yml/.aln) with max safe profanity policy\"
                      git push
                    fi"))
        (BUILD-DOCKER (:ACTION 'RUN :SCRIPT "docker build -t github-aln-bots:latest ."))
        (TAG-IMAGE
          (:ACTION 'RUN
           :SCRIPT "IMAGE_ID=ghcr.io
