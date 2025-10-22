;nyquist plug-in
;version 4
;type generate
;name "ElevenLabs Text-to-Speech"
;author "Audacity Cloud AI"
;release 1.0.0
;copyright "MIT License"
;manpage "Generate speech from text using ElevenLabs AI"

;; ElevenLabs Text-to-Speech Generator for Audacity
;; Generates AI voice-over from text using ElevenLabs Cloud API

;control text "Text to speak" string "" ""
;control voice "Voice ID" string "Rachel" "21m00Tcm4TlvDq8ikWAM"
;control model "Model" choice "Flash v2.5,Turbo v2.5,Multilingual v2" 0
;control stability "Stability" float "" 0.5 0.0 1.0
;control similarity "Similarity Boost" float "" 0.75 0.0 1.0
;control style-level "Style" float "" 0.0 0.0 1.0

;; Map model choice to model ID
(setf model-id
  (case model
    (0 "eleven_flash_v2_5")
    (1 "eleven_turbo_v2_5")
    (2 "eleven_multilingual_v2")
    (t "eleven_flash_v2_5")))

;; Check if text is provided
(if (equal text "")
  (setf error-msg "Please enter text to convert to speech")
  (progn
    ;; Build Python command to generate speech
    (setf plugin-path (get-env "AUDACITY_CLOUDAI_PATH"))
    (if (not plugin-path)
      (setf plugin-path "C:/Coding Projects/Audacity-CloudAI"))
    
    (setf python-cmd (strcat "python \"" plugin-path "/src/generators/text_to_speech.py\""))
    (setf args (format nil " \"~a\" -v \"~a\" -m \"~a\" --stability ~a --similarity ~a --style ~a"
                       text voice model-id stability similarity style-level))
    
    ;; Create temp output file path
    (setf temp-file (strcat plugin-path "/temp_tts.wav"))
    (setf full-cmd (strcat python-cmd args " -o \"" temp-file "\""))
    
    ;; Display info to user
    (setf info-msg (format nil "Generating speech...~%Voice: ~a~%Model: ~a~%Text: ~a"
                          voice model-id (substring text 0 (min 50 (length text)))))
    (print info-msg)
    
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
