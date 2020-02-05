#!r7rs

(import (scheme base)
        (scheme char)
        (csc-151 section)
        (cl testing)
        (williams2-fsa-assignment hawaiian-syllable-fsa))

;;syllable-counter function:
;;parameters: string, a string
;;returns: #f if the string is not a valid hawaiian word
;;         the number of syllables in the word if it is indeed a word
(define syllable-counter (lambda (string)
                           (let ((syllable-count 0)
                                 (str-length (string-length string)))
                             (if (let count-helper ;;recursive loop
                               ((str (string-downcase string))
                                (strlen str-length)
                                (end str-length))
                               (if (> end 0) ;;test for valid substring
                                   (if (syllable-compiled (substring str 0 end))
                                       (begin (set! syllable-count
                                                    (+ 1 syllable-count))
                                              (let* ((newstring (substring str end))
                                                     (length (string-length newstring)))
                                                (count-helper newstring length length)))
                                       (count-helper str strlen (- end 1)))
                                   
                                   ;;test if unprocessed string remains that cannot be classified as a syllable
                                   (if (> (string-length str) 0)
                                       #f
                                       #t)))
                                 
                             (if (eq? 0 syllable-count) ;;test if word contains syllables
                                 #f
                                 syllable-count)
                             #f))))
        
(suite syllable-counter-testing ()
       ;;testing invalid syllables
       (test empty-string
             (syllable-counter "")
             1
             (not))
       (test c-c
             (syllable-counter "kp")
             1
             (not))
       ;;testing single-syllable cases
       (test v
             (syllable-counter "a")
             1
             ((section = <> 1)))
       (test v-long
             (syllable-counter "ee")
             1
             ((section = <> 1)))
       (test c-v
             (syllable-counter "ki")
             1
             ((section = <> 1)))
       (test c-v-long
             (syllable-counter "kuu")
             1
             ((section = <> 1)))
       (test v-v
             (syllable-counter "ai")
             1
             ((section = <> 1)))
       (test v-long-v
             (syllable-counter "eei")
             1
             ((section = <> 1)))
       (test c-v-v
             (syllable-counter "kai")
             1
             ((section = <> 1)))
       (test c-v-long-v
             (syllable-counter "aao")
             1
             ((section = <> 1)))
       (test wuu
             (syllable-counter "wuu")
             1
             ((section = <> 2)))
       ;;testing full words
       (test kilauea
             (syllable-counter "kilauea")
             1
             ((section = <> 4)))
       (test Kilauea
             (syllable-counter "Kilauea")
             1
             ((section = <> 4)))
       (test ahiahi
             (syllable-counter "ahiahi")
             1
             ((section = <> 4)))
       (test uai
             (syllable-counter "uai")
             1
             ((section = <> 2)))
       ;;words with some valid syllables
       (test kilaueapp
             (syllable-counter "kilaueapp")
             1
             (not))
       (test kilappuea
             (syllable-counter "kilappuea")
             1
             (not))
       (test paih
             (syllable-counter "paih")
             1
             (not))
       )