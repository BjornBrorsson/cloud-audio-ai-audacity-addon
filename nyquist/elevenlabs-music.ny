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
    
    ;; Construct full command
    (setf full-cmd (strcat python-cmd args " -o temp_music.wav"))
    
    ;; Display info to user
    (setf info-msg (format nil "Generating music...~%Description: ~a~%Duration: ~a seconds~%Genre: ~a~%Mood: ~a"
                          prompt duration 
                          (if (equal genre "") "Any" genre)
                          (if (equal mood "") "Any" mood)))
    
    ;; Execute the Python script
    ;; Note: This is a simplified version. In practice, you'd need to:
    ;; 1. Execute the Python script
    ;; 2. Read the generated WAV file
    ;; 3. Return the audio data to Audacity
    
    ;; For now, return a simple tone as placeholder
    (s-rest 1.0)
  )
)

;; Return result or error
(if (boundp 'error-msg)
  (s-rest 0)  ;; Return silence on error
  (s-rest 1.0))  ;; Return placeholder audio
