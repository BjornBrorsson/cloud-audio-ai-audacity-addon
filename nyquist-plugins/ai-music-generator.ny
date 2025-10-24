;nyquist plug-in
;version 4
;type generate
;name "AI Music Generator"
;action "Generating music with ElevenLabs..."
;author "Cloud AI for Audacity"
;release 1.0.0
;copyright "MIT License"

;control prompt "Music description" string "" "Upbeat electronic music"
;control duration "Duration (seconds)" int "" 30 5 180
;control prompt-influence "Prompt Influence (0-100)" int "" 30 0 100

;; Build output file path
(setf output-file (format nil "~a\\ai-music-output-~a.wav" 
                         (get-env "TEMP")
                         (get-internal-real-time)))

;; Build the command
(setf cmd (format nil "python audacity_cloudai.py music \"~a\" --duration ~a --prompt-influence ~a -o \"~a\"" 
                  prompt 
                  duration 
                  (/ prompt-influence 100.0)
                  output-file))

;; Execute
(print (strcat "Executing: " cmd))
(system cmd)

;; Wait for generation
(s-rest 0.5)

;; Read and return the audio
(if (not (open output-file :direction :probe))
    (error "Could not generate music" "Check Python backend and API key")
    (s-read output-file))
