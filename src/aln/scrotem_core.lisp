;;; ====================================================
;;; Sro’tem Sticky Enforcement Procedure
;;; Purpose:
;;;   - Ensures horror/roleplay/adult files are only loaded
;;;     in approved directories.
;;;   - Verifies mandatory metadata headers exist and contain
;;;     correct values.
;;;   - Acts as "guardrails" to prevent breaking narrative/
;;;     horror integrity during execution.
;;; ========================================================
;;; --- 0. Constants & Policy Configuration ---
approved_dirs := [
    "src/aln",
    "modules",
    "lisp/registry",
    "npc",
    "narrative",
    "game",
    "debugging"
]
required_headers := [
    "aln_logic_parent",
    "sticky_horror",
    "content_rating",
    "horror_compliance",
    "last_audit"
]
mandatory_values := {
    "sticky_horror": "true",
    "content_rating": "Adults-Only"
}
;;; --- 1. Directory Compliance Check ---
dir_ok := FALSE
FOR dir IN approved_dirs DO
    IF PATH.BEGINS_WITH(filepath, dir) THEN
        dir_ok := TRUE
        BREAK
    ENDIF
ENDFOR
IF NOT dir_ok THEN
    LOG.ERROR("BLOCKED: Sticky-Trigger Violation ❌ '~filepath~' outside approved dirs.")
    RETURN FAIL("Sticky directory policy violation: ~filepath~")
ENDIF
;;; --- 2. Metadata Header Extraction (first 20 lines) ---
filelines := FILE.READ_LINES(filepath, 1, 20)
IF LIST.LENGTH(filelines) == 0 THEN
    LOG.ERROR("BLOCKED: '~filepath~' is empty or unreadable.")
    RETURN FAIL("File unreadable/empty: ~filepath~")
ENDIF
metadata := DICT.EMPTY()
FOR line IN filelines DO
    IF REGEX.MATCH(line, "^[#;:]+[ \t]*([a-zA-Z_-]+):[ \t]*[\"']?([^\"']*)") THEN
        parts := REGEX.CAPTURE_ALL(line, "^[#;:]+[ \t]*([a-zA-Z_-]+):[ \t]*[\"']?([^\"']*)")
        key := STRING.LOWER(STRING.TRIM(parts[1]))
        val := STRING.TRIM(parts[2])
        metadata[key] := val
    ENDIF
ENDFOR
;;; --- 3. Metadata Compliance Enforcement ---
FOR header IN required_headers DO
    IF NOT DICT.HAS_KEY(metadata, header) OR metadata[header] == "" THEN
        LOG.ERROR("BLOCKED: '~filepath~' missing required sticky metadata '~header~'.")
        RETURN FAIL("Sticky metadata field '~header~' missing in ~filepath~")
    ENDIF
ENDFOR
;;; --- 4. Mandatory Value Enforcement ---
FOR k, v IN mandatory_values DO
    IF metadata[k] != v THEN
        LOG.ERROR("BLOCKED: '~filepath~' fails sticky-value check ('~k~' must be '~v~').")
        RETURN FAIL("Sticky-value mismatch in '~filepath~' for '~k~'")
    ENDIF
ENDFOR
;;; --- 5. Extra Narrative/Roleplay Safety Checks ---
IF STRING.CONTAINS(metadata["aln_logic_parent"], "deprecated") THEN
    LOG.WARN("WARNING: '~filepath~' uses deprecated logic parent.")
ENDIF
;;; Optional: track audit timestamps strictly
IF NOT TIME.IS_VALID(metadata["last_audit"]) THEN
    LOG.ERROR("BLOCKED: '~filepath~' missing valid last_audit timestamp.")
    RETURN FAIL("Invalid audit timestamp for '~filepath~'")
ENDIF
;;; --- 6. Pass ✅ ---
LOG.INFO("PASSED: Sticky compliance OK for '~filepath~'")
RETURN OK()
ENDPROC
;;; Example invocation:
;;; enforce_sticky_trigger("src/aln/procedure/my_horror_script.aln")
