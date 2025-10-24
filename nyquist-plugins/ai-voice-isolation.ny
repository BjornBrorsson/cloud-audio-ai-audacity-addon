;nyquist plug-in
;version 4
;type process
;name "AI Voice Isolation"
;action "Isolating voice with ElevenLabs..."
;author "Cloud AI for Audacity"
;release 1.0.0
;copyright "MIT License"

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

;; Save the selected audio to a temp file
(setf input-file (format nil "~a\\ai-isolation-input-~a.wav" 
                        (get-env "TEMP")
                        (get-internal-real-time)))

(setf output-file (format nil "~a\\ai-isolation-output-~a.wav" 
                         (get-env "TEMP")
                         (get-internal-real-time)))

;; Write the selected audio to input file
(s-save *track* ny:all input-file)

;; Build the command
(setf cmd (format nil "python audacity_cloudai.py isolate \"~a\" -o \"~a\"" 
                  input-file 
                  output-file))

;; Execute
(print (strcat "Executing: " cmd))
(system cmd)

;; Wait for processing
(s-rest 0.5)

;; Read and return the processed audio
(if (not (open output-file :direction :probe))
    (error "Could not isolate voice" "Check Python backend and API key")
    (s-read output-file))
