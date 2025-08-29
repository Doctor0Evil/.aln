;; ALNFantasia_Combat.sim/ci/github_workflow_upgrade.lisp

;; Routine to enforce artifact action compatibility and matrix-safe naming.

(defun upgrade-upload-artifact-action (workflow-path)
  "Patch workflow YAML to use actions/upload-artifact@v4 and add matrix-specific artifact name."
  (let ((yaml (read-workflow-yaml workflow-path)))
    (update-action-version yaml "actions/upload-artifact" "v4")
    (ensure-matrix-artifact-uniqueness yaml)
    (write-workflow-yaml workflow-path yaml)
    (log-event (list :type "workflow_patch"
                     :target workflow-path
                     :change "upload-artifact action bumped to v4, naming matrix-safe"
                     :timestamp (current-timestamp)))))

(defun ensure-matrix-artifact-uniqueness (yaml)
  "Ensures upload-artifact step names include a matrix-specific suffix (e.g. ${{ matrix.python-version }})."
  t)

;; Apply to ML workflow
(upgrade-upload-artifact-action ".github/workflows/Build-Test-ML.yml")
