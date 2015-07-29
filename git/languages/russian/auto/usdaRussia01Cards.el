(TeX-add-style-hook "usdaRussia01Cards"
 (lambda ()
    (TeX-add-symbols
     '("aDlog" 1)
     '("speakE" 1)
     '("speakR" 1)
     '("inR" 1))
    (TeX-run-style-hooks
     "dialogue"
     "babel"
     "russian"
     "english"
     "inputenc"
     "utf8"
     "fontenc"
     "T2A"
     "T1"
     "latex2e"
     "flashcards10"
     "flashcards"
     "avery5371"
     "grid"
     "frame")))

