;; file path in repo:
;; https://github.com/Doctor0Evil/ALN_Programming_Language.git/chaos/roach-boss-engage.lisp

(defun engage-roach-boss-speech (trigger)
  "Main function controlling the Roach Boss engage dialogue based on trigger events"
  (format t "~%[Roach Boss, slick in a grease-stained tracksuit, cracks a bottle over a rusty gear. A cologne cloud of citronella and Olde English surrounds him. His antennae twitch at the player.]~%")
  ;; Decision tree of trigger responses
  (case trigger
    
    (:user-interaction
      (format t "~%ROACH BOSS: Who dares disturb my trash throne, eh? I was mid-siesta!~
        ~%You looking for scrap, loot, or just a whiff of my glory?~
        ~%If it’s loot you want, you’ll have to dance the 'Bin-Dusta Shuffle.'~
        ~%Or bribe me with a sixer—your move, freakshow.~
        ~%...See that drooler in the pit? That’s Carl. Don’t step on ‘im, he’s my best mate.~
        ~%Sometimes Carl thinks *he’s* the boss. Adorable, right? HA!~
        ~%Now fork over the bottle... or wrestle the compactor until the gods decide your limb count.~
        ~%...Or stay. Lemme tell ya ‘bout the Great Liberty Freak Waster Assault of ‘92.~
        ~%I itched for three months in places you can’t even spell.~"))
    
    (:fail-roach-exe
      (format t "~%[Roach Boss spazzes violently, mandibles clicking against metal pipes.]~
        ~%ROACH BOSS: Aw bug-guts, not again… system gone sideways!~
        ~%Assemble the crew—shift’s over till I get a double shot o’ Bugshine and my rusted crutch!~
        ~%Carl, wake your drunk thorax up—we got OUTSIDERS!~
        ~%And someone better smuggle in enough RAID ‘cause I’m not dying sober!~"))
    
    (:aggro
      (format t "~%[Roach Boss slams his bottle against the valve wheel, froth hissing across the floor.]~
        ~%ROACH BOSS: You WANT trouble? You GOT trouble!~
        ~%I’ll twist yer bones into rebar and floss my mandibles with your nerves!~
        ~%Carl, take his shoes—HE WON’T NEED ‘EM WHERE HE’S GOING!~"))
    
    (t
      (format t "~%ROACH BOSS: If you’re not working, you’re part of the waste-stream.~
        ~%Choose wisely, trashling.~"))))
