;nyquist plug-in
;version 4
;type generate
;name "AI Text-to-Speech"
;action "Generating speech with ElevenLabs..."
;author "Cloud AI for Audacity"
;release 1.0.0
;copyright "MIT License"

;control text "Text to speak" string "" "Enter your text here"
;control voice "Voice ID (optional)" string "" ""
;control model "Model" choice "Turbo v2.5,Flash v2.5,Multilingual v2" 0
;control stability "Stability (0-100)" int "" 50 0 100
;control similarity "Similarity Boost (0-100)" int "" 75 0 100
;control style "Style Exaggeration (0-100)" int "" 0 0 100

;; Build output file path in system temp directory
(setf output-file (format nil "~a\\ai-tts-output-~a.wav" 
                         (get-env "TEMP")
                         (get-internal-real-time)))

;; Build the command to call Python backend
(setf cmd (format nil "python audacity_cloudai.py tts \"~a\"" text))

;; Add optional voice ID
(if (> (length voice) 0)
    (setf cmd (strcat cmd " --voice \"" voice "\"")))

;; Add model selection
(setf model-names (list "eleven_turbo_v2_5" "eleven_flash_v2_5" "eleven_multilingual_v2"))
(setf model-id (nth model model-names))
(setf cmd (strcat cmd " --model \"" model-id "\""))

;; Add voice settings
(setf cmd (strcat cmd 
                  " --stability " (format nil "~a" (/ stability 100.0))
                  " --similarity " (format nil "~a" (/ similarity 100.0))
                  " --style " (format nil "~a" (/ style 100.0))))

;; Add output file
(setf cmd (strcat cmd " -o \"" output-file "\""))

;; Execute the Python script
(print (strcat "Executing: " cmd))
(system cmd)

;; Wait a moment for file to be written
(s-rest 0.5)

;; Read and return the generated audio
(if (not (open output-file :direction :probe))
    (error "Could not generate audio" "Make sure Python backend is installed and ElevenLabs API key is set")
    (s-read output-file))
