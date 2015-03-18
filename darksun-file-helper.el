(defun file-size(file-path)
  "获取文件的字节数"
  (nth 7 (file-attributes file-path)))

(defun file-concat(dir file)
  "连接文件路径"
  (concat (file-name-as-directory dir) file))

(defun file-md5(path)
  "计算文件内容的md5"
  (with-temp-buffer
	(insert-file-contents-literally path)
	(md5 (buffer-string))))

(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where this function is called.

摘自李杀网
Example: If you have this line
 (fullpath-relative-to-current-file \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

 ① If you have file A, that calls the `load' on a file at B, and
B calls “load” on file C using a relative path, then Emacs will
complain about unable to find C. Because, emacs does not switch
current directory with “load”.

 To solve this problem, when your code only knows the relative
path of another file C, you can use the variable `load-file-name'
to get the current file's full path, then use that with the
relative path to get a full path of the file you are interested.

 ② To know the current file's full path, emacs has 2 ways:
`load-file-name' and `buffer-file-name'. If the file is loaded
by “load”, then load-file-name works but buffer-file-name
doesn't. If the file is called by `eval-buffer', then
load-file-name is nil. You want to be able to get the current
file's full path regardless the file is run by “load” or
interactively by “eval-buffer”."
  (file-concat (or load-file-name buffer-file-name) file-relative-path))
