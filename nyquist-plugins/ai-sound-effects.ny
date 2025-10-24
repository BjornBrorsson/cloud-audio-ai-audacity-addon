;nyquist plug-in
;version 4
;type generate
;name "AI Sound Effects"
;action "Generating sound effects with ElevenLabs..."
;author "Cloud AI for Audacity"
;release 1.0.0
;copyright "MIT License"

;control prompt "Sound description" string "" "Door creaking open"
;control duration "Duration (seconds)" int "" 10 1 60
;control prompt-influence "Prompt Influence (0-100)" int "" 30 0 100

;; Check for API key first
(setf plugin-dir (get-env "APPDATA"))
(setf plugin-dir (strcat plugin-dir "\\audacity\\Plug-Ins"))
(setf env-file (strcat plugin-dir "\\.env"))

;; Check if .env file exists
(setf has-env (open env-file :direction :probe))
(if (not has-env)
    (error "API Key Required" 
           (strcat "Get a free API key at: https://elevenlabs.io"
                   "\n\nThen create a file named .env in:"
                   "\n" plugin-dir
                   "\n\nWith this content:"
                   "\nELEVENLABS_API_KEY=your_key_here"
                   "\n\nOr run: install-nyquist-plugins.ps1 again")))

;; Build output file path
(setf output-file (format nil "~a\\ai-sfx-output-~a.wav" 
                         (get-env "TEMP")
                         (get-internal-real-time)))

;; Build the command
(setf cmd (format nil "python audacity_cloudai.py sfx \"~a\" --duration ~a --prompt-influence ~a -o \"~a\"" 
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
    (error "Could not generate sound effect" "Check Python backend and API key")
    (s-read output-file))
