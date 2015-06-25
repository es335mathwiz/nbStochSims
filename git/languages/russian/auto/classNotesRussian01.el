(TeX-add-style-hook "classNotesRussian01"
 (lambda ()
    (LaTeX-add-labels
     "sec:june-24"
     "sec:group-letters")
    (TeX-add-symbols
     '("inR" 1))
    (TeX-run-style-hooks
     "babel"
     "russian"
     "english"
     "inputenc"
     "utf8"
     "fontenc"
     "T2A"
     "latex2e"
     "art10"
     "article")))

