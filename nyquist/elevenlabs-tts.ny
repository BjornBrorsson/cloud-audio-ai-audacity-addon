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
    
    ;; Construct full command
    (setf full-cmd (strcat python-cmd args " -o temp_tts.wav"))
    
    ;; Display info to user
    (setf info-msg (format nil "Generating speech...~%Voice: ~a~%Model: ~a~%Text: ~a"
                          voice model-id (substring text 0 (min 50 (length text)))))
    
    ;; Execute the Python script
    ;; Note: This is a simplified version. In practice, you'd need to:
    ;; 1. Execute the Python script
    ;; 2. Read the generated WAV file
    ;; 3. Return the audio data to Audacity
    
    ;; For now, return a simple tone as placeholder
    ;; (In a full implementation, this would load and return the generated audio)
    (s-rest 1.0)
  )
)

;; Return result or error
(if (boundp 'error-msg)
  (s-rest 0)  ;; Return silence on error
  (s-rest 1.0))  ;; Return placeholder audio
