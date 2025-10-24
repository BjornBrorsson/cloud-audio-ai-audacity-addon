;nyquist plug-in
;version 4
;type analyze
;name "AI Transcription"
;action "Transcribing audio with ElevenLabs..."
;author "Cloud AI for Audacity"
;release 1.0.0
;copyright "MIT License"

;control language "Language" choice "Auto,English,Spanish,French,German,Italian,Portuguese,Polish,Turkish,Russian,Dutch,Swedish,Filipino,Japanese,Ukrainian,Greek,Czech,Finnish,Romanian,Danish,Bulgarian,Malay,Slovak,Croatian,Arabic,Tamil,Hindi" 0
;control format "Output format" choice "Text,SRT,JSON,Labels" 0

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
(setf input-file (format nil "~a\\ai-transcribe-input-~a.wav" 
                        (get-env "TEMP")
                        (get-internal-real-time)))

(setf output-file (format nil "~a\\ai-transcribe-output-~a.txt" 
                         (get-env "TEMP")
                         (get-internal-real-time)))

;; Write the selected audio to input file
(s-save *track* ny:all input-file)

;; Map language index to code
(setf language-codes (list "auto" "en" "es" "fr" "de" "it" "pt" "pl" "tr" "ru" "nl" "sv" "fil" "ja" "uk" "el" "cs" "fi" "ro" "da" "bg" "ms" "sk" "hr" "ar" "ta" "hi"))
(setf lang-code (nth language language-codes))

;; Map format index to format string
(setf format-types (list "text" "srt" "json" "labels"))
(setf format-type (nth format format-types))

;; Build the command
(setf cmd (format nil "python audacity_cloudai.py transcribe \"~a\" --language ~a --format ~a -o \"~a\"" 
                  input-file 
                  lang-code
                  format-type
                  output-file))

;; Execute
(print (strcat "Executing: " cmd))
(system cmd)

;; Wait for processing
(s-rest 1.0)

;; Read and display the transcription
(if (not (open output-file :direction :probe))
    (error "Could not transcribe audio" "Check Python backend and API key")
    (progn
      (setf transcript (get-file-contents output-file))
      (format nil "Transcription:~%~a" transcript)))
