;nyquist plug-in
;version 4
;type process
;name "AI Voice Isolation"
;action "Isolating voice with ElevenLabs..."
;author "Cloud AI for Audacity"
;release 1.0.0
;copyright "MIT License"

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
