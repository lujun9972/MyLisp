(elake-task say-hello (wash) 
  "say hello to "
  (message "hello"))
(elake-task wash (file:wash) 
  "wash faces"
  (message "wash"))
(elake-task file:wash (file:bowl)
  (shell-command "touch wash"))
(elake-task file:bowl ()
  (shell-command "touch bowl"))
(elake-task go-out (say-hello  wash)
  (message "go out"))
