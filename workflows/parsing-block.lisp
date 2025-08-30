(defblock parsing.block
  ;; Manifest toggle check
  (when (manifest:enabled? 'aln-parsing)
    (log:info "ALN parsing enabled via manifest toggle.")

    ;; Input validation
    (let ((input (fetch:artifact "aln.raw"))
          (schema (load:schema "aln.schema")))
      (assert (not (null input)) "Missing ALN input artifact.")
      (assert (schema:valid? input schema) "ALN input failed schema validation.")

      ;; Parse and normalize
      (let ((parsed (aln:parse input)))
        (log:debug "Parsed ALN content:" parsed)

        ;; Audit hook
        (audit:record 'aln-parsing
                      :input input
                      :output parsed
                      :timestamp (time:now)
                      :bot-id (bot:self))

        ;; Conditional transformation
        (when (manifest:enabled? 'aln-transform)
          (let ((transformed (aln:transform parsed)))
            (log:info "ALN content transformed.")
            (store:artifact "aln.transformed" transformed)))

        ;; Recovery fallback
        (unless parsed
          (log:error "ALN parsing failed. Triggering fallback.")
          (trigger:bot 'fallback-handler :context 'aln-parsing))))

  ;; Disabled toggle path
  (unless (manifest:enabled? 'aln-parsing)
    (log:warn "ALN parsing skipped due to manifest toggle.")))
