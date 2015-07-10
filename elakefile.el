(elake-task say-hello (wash) 
  "say hello to "
  (message "hello"))
(elake-task wash () 
  "wash faces"
  (message "wash"))

(elake-task go-out (say-hello  wash)
  (message "go out"))
