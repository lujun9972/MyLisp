(defun read-sudoku-from-file (file)
  "read sudoku from file"
  (with-temp-buffer
	(insert-file-contents file)
	(read-from-whole-string (buffer-string))))

(defun get-row-data-from-sudoku (sudoku row)
  "获取数独中的第`row'行数字,`row'从0开始"
  (nth row sudoku))

(defun get-col-data-from-sudoku (sudoku col)
  "获取数独中的第`col'行数字,`col'从0开始"
  (mapcar (lambda (row-data)
			(nth col row-data))
		  sudoku))

(defun get-element-from-sudoku (sudoku row col)
  ""
  (nth col (get-row-from-sudoku sudoku row)))

(setq sudoku '((1 2 3 nil)
			   (2 nil 0 1)
			   (3 nil 1 2)
			   (0 1 2 3)))

(defun gen-full-data-from-sudoku (sudoku)
  (let ((size (length sudoku))
		full-data)
	(dotimes (num size full-data)
	  (push num full-data))))


(defun sudoku-solution-guesser (sudoku row col)
  "推测sudoku中第row行第col列可以是哪个数字"
  (if (get-element-from-sudoku sudoku row col)
	  (list (get-element-from-sudoku sudoku row col))
	(let* ((row-data (get-row-data-from-sudoku sudoku row))
		   (col-data (get-col-data-from-sudoku sudoku col))
		   (full-data (gen-full-data-from-sudoku sudoku))
		   (row-candidate (cl-set-difference full-data row-data))
		   (col-candidate (cl-set-difference full-data col-data))
		   (candidate (cl-intersection row-candidate col-candidate)))
	  candidate)))

