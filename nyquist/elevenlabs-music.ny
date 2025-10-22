;nyquist plug-in
;version 4
;type generate
;name "ElevenLabs Music Generation"
;author "Audacity Cloud AI"
;release 1.0.0
;copyright "MIT License"
;manpage "Generate music from text using ElevenLabs AI"

;; ElevenLabs Music Generator for Audacity
;; Generates AI music from text descriptions using ElevenLabs Cloud API

;control prompt "Music description" string "" ""
;control duration "Duration (seconds)" int "" 30 10 120
;control genre "Genre (optional)" string "" ""
;control mood "Mood (optional)" string "" ""
;control use-plan "Use composition plan" choice "No,Yes" 0

;; Check if prompt is provided
(if (equal prompt "")
  (setf error-msg "Please enter a music description")
  (progn
    ;; Build Python command to generate music
    (setf plugin-path (get-env "AUDACITY_CLOUDAI_PATH"))
    (if (not plugin-path)
      (setf plugin-path "C:/Coding Projects/Audacity-CloudAI"))
    
    (setf python-cmd (strcat "python \"" plugin-path "/src/generators/music_generator.py\""))
    (setf args (format nil " \"~a\" -d ~a" prompt duration))
    
    ;; Add optional parameters
    (if (not (equal genre ""))
      (setf args (strcat args " --genre \"" genre "\"")))
    
    (if (not (equal mood ""))
      (setf args (strcat args " --mood \"" mood "\"")))
    
    (if (= use-plan 1)
      (setf args (strcat args " --plan")))
    
    ;; Create temp output file path
    (setf temp-file (strcat plugin-path "/temp_music.wav"))
    (setf full-cmd (strcat python-cmd args " -o \"" temp-file "\""))
    
    ;; Display info to user
    (setf info-msg (format nil "Generating music...~%Description: ~a~%Duration: ~a seconds~%Genre: ~a~%Mood: ~a"
                          prompt duration 
                          (if (equal genre "") "Any" genre)
                          (if (equal mood "") "Any" mood)))
    (print info-msg)
    (print "This may take 30-60 seconds...")
    
    ;; Execute the Python script and wait for it to finish
    ;; Note: (system) may be disabled in some Audacity builds for security
    (setf exit-code (system full-cmd))
    
    ;; Check if command executed successfully
    (if (= exit-code 0)
      (progn
        ;; Try to read the generated audio file
        (setf *track* (s-read temp-file))
        (if *track*
          *track*  ;; Return the generated audio
          (progn
            (print "Error: Could not read generated audio file")
            (s-rest 0))))
      (progn
        (print (format nil "Error: Python script failed with exit code ~a" exit-code))
        (print "Make sure Python and dependencies are installed.")
        (print "Set AUDACITY_CLOUDAI_PATH environment variable if needed.")
        (s-rest 0)))
  )
)
