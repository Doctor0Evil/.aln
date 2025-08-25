;;; ====================================================
;;; SCRO'TEM RESPONSE INJECTOR — v0.2
;;; ====================================================
(define (ScroTem-INJECTOR input_context)
  ;; INPUT_CONTEXT: narrative_stub, provoke_flags, emotional_valence
  ;; RETURNS: generated_response + debug_payload

  (let* (
        ;; -- RAW TRIGGERS -----------------
        (scene.location  "SLUDGE-CHURNED CUBICLE PIT — SLAUGHTERHOUSE DISTRICT")
        (scene.time      "03:17 AM")
        (scene.mood      'decay/feral-grotesque)
        (scene.lighting  'flickering_tubes)
        (provoke.target   "slimy wart-encrusted sack of rancid giggles")
        (stimulus.type    'mockery)
        (stimulus.flags   '(taunt insult trivialize_work compare-specimens))
        
        ;; -- CHARACTER EMOTIONAL DIALS -----
        (emotion.primary 'wrath)
        (emotion.secondary 'mockery_backlash)
        (tone.register    'pustule_slasher_pantomime)
        (theme.keywords   '(rot scrotum decay meat_nightmare corporate_parody))

        ;; -- NARRATIVE DEV PATH ------------
        (branch.selected 'AMPLIFY_GROTESQUE_VS_TAUNT)
        (logic.mode       'overindulgent_tirade)
        (harm_factor      'social_psychophysical)
        (punchline.inject  t)
        
        ;; -- OUTPUT ------------------------
        (response.text
"""
Oh, you slimy, wart-encrusted sack of rancid giggles—ha yourself, you festering boil on the ass of oblivion!... [SEQUENCE CONTINUES AS USER PROVIDED TEXT HERE]
"""
        )

        (debug.payload
          '(
            (trigger.root
              ("user-tone evaluated as sheer provocation"
               "context-saturated with grotesque imagery"
               "taunt -> escalate grotesque arms race"))
            (logic.chosen-path '(Insultescalation Horror-Elevation PunchlineEmbedding))
            (filters.applied '(none_explicitly_removed gore-allowed ironic-satire-pass))
            (sequence.expansion
              ("Evaluate Insult Struct -> choose Grotesque Adjective Banks"
               "Load Macabre_Workplace_Patterns"
               "Insert Character-Violence Memory_Banks"
               "Weave with Recurring Motif: gorilla_nut_sacks / pus-drain feasts"
               "Close Loop: invoke SCROTY Ritual Cathedral"
              ))
            (comedic.injectors
              ("rectangle_forehead" -> scapegoat-effigy
               "paperweight citation" -> bureaucratic slapstick
               "loaf_of_scroty" -> ritualistic surreal punch))
            (output.verification
              ("response-length OK"
               "tone-level flagged Beyond-Horror"
               "delight factor 82%"
               "injection readiness: TRUE"))
          ))
        )
    response.text))
