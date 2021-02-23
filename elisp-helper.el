;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(load "subr")
(defun function-arity (fn)
  "获取函数`fn'所允许的最小和最大参数个数,参见`subr-arity'但fn可以是任意函数"
  (setq fn (indirect-function fn))
  (if (subrp fn)
	  (subr-arity fn)
	(require 'help)
	(require 'cl-lib)
	(let* ((fn-arglist (help-function-arglist fn))
		   (min-arg (or (cl-position-if (lambda (x)
										  (member x '(&optional &rest))) fn-arglist)
						(length fn-arglist)))
		   (max-arg (cond ((member '&rest fn-arglist)
						   'many)
						  ((member '&optional fn-arglist)
						   (1- (length fn-arglist)))
						  (t min-arg))))
	  (cons min-arg max-arg))))
;; Onlisp中的列表工具
(cl-proclaim '(inline last1 single append1 conc1 mklist))
(defun last1 (lst)
  (car (last lst)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj)
  (append lst (list obj)))
(defun conc1 (lst obj)
  (nconc lst (list obj)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))
(defun longer (x y)
  (cl-labels ((compare (x y)
					(and (consp x)
						 (or (null y)
							 (compare (cdr x) (cdr y))))))
	(if (and (listp x) (listp y))
		(compare x y)
	  (> (length x) (length y)))))
(defun filter (fn lst)
  (let ((acc nil))
	(dolist (x lst)
	  (let ((val (funcall fn x)))
		(if val (push val acc))))
	(nreverse acc)))
(defun group (source n)
  "将source以n个元素一组地分组"
  (if (zerop n) (error ”zero length”))
  (cl-labels ((rec (source acc)
				(let ((rest (nthcdr n source)))
				  (if (consp rest)
					  (rec rest (cons (subseq source 0 n) acc))
					(nreverse (cons source acc))))))
	(if source (rec source nil) nil)))
(defun flatten (x)
  "返回由一个列表中的所有原子(atom)，或者说是元素的元素所组成的列表"
  (cl-labels ((rec (x acc)
				(cond ((null x) acc)
					  ((atom x) (cons x acc))
					  (t (rec (car x) (rec (cdr x) acc))))))
	(rec x nil)))
(defun prune (test tree)
  "prune，它对remove-if的意义就相当于copy-tree之于copy-list。也就是说，它会向下递归到子列表里"
  (cl-labels ((rec (tree acc)
				(cond ((null tree) (nreverse acc))
					  ((consp (car tree))
					   (rec (cdr tree)
							(cons (rec (car tree) nil) acc)))
					  (t (rec (cdr tree)
							  (if (funcall test (car tree))
								  acc
								(cons (car tree) acc)))))))
	(rec tree nil)))
;; OnLisp中的搜索工具
(defun find2 (fn lst)
  (if (null lst)
	  nil
	(let ((val (funcall fn (car lst))))
	  (if val
		  (values (car lst) val)
		(find2 fn (cdr lst))))))
(cl-defun before-p (x y lst &key (test #'eql))
  "它告诉你在列表`lst'中的`x'是否在`y'的前面"
  (and lst
	   (let ((first (car lst)))
		 (cond ((funcall test y first) nil)
			   ((funcall test x first) lst)
			   (t (before x y (cdr lst) :test test))))))
(cl-defun after-p (x y lst &key (test #'eql))
  "它告诉你在列表`lst'中的`x'是否在`y'的后面"
  (let ((rest (before y x lst :test test)))
	(and rest (member x rest :test test))))
(cl-defun duplicate-p (obj lst &key (test #'eql))
  "`obj'在`lst'是否有多个"
  (member obj (cdr (member obj lst :test test))
		  :test test))
(defun split-if (fn lst)
  "member的某种泛化。不同之处在于member先搜索想要找的元素，然后返回从找到元素开始的列表的cdr，而split-if把原列表的两个部分都返回了。该实用工具主要用于已经按照某种规则排好序的列表:"
  (let ((acc nil))
	(cl-do ((src lst (cdr src)))
		((or (null src) (funcall fn (car src)))
		 (values (nreverse acc) src))
	  (push (car src) acc))))

(defun most (fn lst)
  "most接受一个列表和一个用来打分的函数，其返回值是列表中分数最高的元素。分数相等的时候，排在前面的元素优先.为了方便调用方，most也返回了获胜元素的分数"
  (if (null lst)
	  (values nil nil)
	(let* ((wins (car lst))
		   (max (funcall fn wins)))
	  (dolist (obj (cdr lst))
		(let ((score (funcall fn obj)))
		  (when (> score max)
			(setq wins obj
				  max score))))
	  (values wins max))))

(defun best (cmp lst)
  "best提供了一种更通用的搜索方式。该实用工具接受一个函数和一个列表，但这里的函数必须是个两参 数谓词。它返回的元素在该谓词下胜过所有其他元素。
我们可以认为best等价于sort的car, 但前者的效率更高些"
  (if (null lst)
	  nil
	(let ((wins (car lst)))
	  (dolist (obj (cdr lst))
		(if (funcall cmp obj wins)
			(setq wins obj)))
	  wins)))

(defun mostn (fn lst)
  "mostn接受一个函数和一个列表，并返回一个由获得最高分的所有元素组成的列表"
  (if (null lst)
	  (values nil nil)
	(let ((result (list (car lst)))
		  (max (funcall fn (car lst))))
	  (dolist (obj (cdr lst))
		(let ((score (funcall fn obj)))
		  (cond ((> score max)
				 (setq max score
					   result (list obj)))
				((= score max)
				 (push obj result)))))
	  (values (nreverse result) max))))
;; OnLisp中的映射函数
(defun map0-n (fn n)
  "map `fn' 从0到`n'"
  (mapa-b fn 0 n))
(defun map1-n (fn n)
  "map `fn' 从1到`n'"
  (mapa-b fn 1 n))
(cl-defun mapa-b (fn a b &optional (step 1))
  "map `fn' 从`a' 到`b',每次步进`step'"
  (cl-do ((i a (+ i step))
	   (result nil))
	  ((> i b) (nreverse result))
	(push (funcall fn i) result)))
(defun map-> (fn start test-fn succ-fn)
  "map `fn' 从`start' 到`test-fn'为真为止,每次步进通过调用`succ-fn'完成"
  (cl-do ((i start (funcall succ-fn i))
	   (result nil))
	  ((funcall test-fn i) (nreverse result))
	(push (funcall fn i) result)))
(defun mappend (fn &rest lsts)
  "mapcan的非破坏版本"
  (apply #'append (apply #'mapcar fn lsts)))
(defun mapcars (fn &rest lsts)
  "mapcar作用到(apply #'append list)中"
  (let ((result nil))
	(dolist (lst lsts)
	  (dolist (obj lst)
		(push (funcall fn obj) result)))
	(nreverse result)))
(defun rmapcar (fn &rest args)
  "适于树的mapcar,且cl-mapcar一样，它可以接受一个以上的列表作为参数,例如
 (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40))) => (11 (22 (33) 44)) "
  (if (some #'atom args)
	  (apply fn args)
	(apply #'cl-mapcar
			  #'(lambda (&rest args)
				   (apply #'rmapcar fn args))
				 args)))

;; (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))
;; OnLisp中的字符串函数
(defun mkstr (&rest args)
  "mkstr，它接受任意数量的参数，并将它们的打印形式连起来，形成一个字符串"
  (with-output-to-string (s)
						 (dolist (a args) (princ a s))))
(defun symb (&rest args)
  "symb，大多数情况下，它被用来构造符号。它接受一个或多个参数，并返回一个符号(若需要的话，则会新建一个)，使其打印名称等于所有参数连接在一起的字符串。它可以接受任何支持可打印表示的对象作为参数: 符号、字符串、数字，甚至列表。"
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  "reread，是symb的通用化版本。它接受一系列对象，然后打印并重读它们。它可以像symb那样返回符号，但也可以返回其他任何read能返回的东西。其间，读取宏将会被调用，而不是被当成函数的一部分"
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  "explode接受一个符号，然后返回一个由该符号名称里的字符所组成的列表"
  (map 'list #'(lambda (c)
				 (intern (make-string 1 c)))
	   (symbol-name sym)))
;; onlisp中关于函数的辅组工具
(defvar *!equivs* (make-hash-table)
  "存放一般函数与其破坏性函数的对应关系")

(defun ! (fn)
  "获取`fn'对应的破坏性函数"
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  "定义`fn'的破坏性函数为`fn!'"
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  "返回`fn'的带缓存功能的相应版本的函数"
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (if (member args (hash-table-keys cache))
          (gethash args cache)
        (setf (gethash args cache)
              (apply fn args))))))

(defun compose-fns (&rest fns)
  "组合多个函数"
  (if fns
	  (lexical-let ((fn1 (car (last fns)))
			(fns (butlast fns)))
		(lambda (&rest args)
			 (reduce #'funcall fns
						:from-end t
						:initial-value (apply fn1 args))))
	#'identity))

(defun complement (pred)
  "返回判断函数`pred'的互补函数not-pred"
  (compose-fns #'not pred))
(defun fif (if then &optional else)
  (lambda (x)
	(if (funcall if x)
		(funcall then x)
	  (if else (funcall else x)))))

(defun fint (fn &rest fns)
  "function intersection
(funcall (fint fn1 fn2 fn3) x)等价于(and (fn1 x) (fn2 x) (fn3 x))"
  (if (null fns)
	  fn
	(let ((chain (apply #'fint fns)))
	  (lambda (x)
		(and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  " (funcall (fint fn1 fn2 fn3) x)等价于(or (fn1 x) (fn2 x) (fn3 x))"
  (if (null fns)
	  fn
	(let ((chain (apply #'fun fns)))
	  (lambda (x)
		(or (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
  "list recurser,对列表上的cdr进行递归操作
lrec的第一个参数必须是一个接受两个参数的函数，一个参数是列表的当前car，另一个参数是个函数，通 过调用这个函数，递归得以进行。"
  (cl-labels ((self (lst)
				 (if (null lst)
					 (if (functionp base)
						 (funcall base)
					   base)
				   (funcall rec (car lst)
							#'(lambda ()
								 (self (cdr lst)))))))
	#'self))

(cl-defun ttrav (rec &optional (base #'identity))
  "对数上的叶子节点递归操作.

对各叶子节点调用base操作
对car和cdr子树,使用rec函数整合,(rec (car  tree) (cdr tree))"
  (cl-labels ((self (tree)
				 (if (atom tree)
					 (if (functionp base)
						 (funcall base tree)
					   base)
				   (funcall rec (self (car tree))
							(if (cdr tree)
								(self (cdr tree)))))))
	#'self))

(cl-defun tree-collect-subtree-if (pred tree)
  "收集tree中所有符合条件的子树"
  (when tree
	(cond ((atom tree)
		   (when (funcall pred tree)
			 (list tree)))
		  ((funcall pred tree)
		   (append (list tree)
				   (tree-collect-subtree-if pred (car tree))
				   (tree-collect-subtree-if pred (cdr tree))))
		  (t (append (tree-collect-subtree-if pred (car tree))
					 (tree-collect-subtree-if pred (cdr tree)))))))

(cl-defun trec (rec &optional (base #'identiy))
  "trec是一个更通用的树结构递归操作函数的生成器, 这种函数生成器能让我们控制递归调用发生的时机，以及是否继续递归。
trec的第一个参数应当是一个具有三个参数的函数，三个参数分别是: 当前的对象，以及两个递归调用。后两个参数将是用来表示对左子树和右子树进行递归的两个闭包。"
  (cl-labels
	  ((self (tree)
			 (if (atom tree)
				 (if (functionp base)
					 (funcall base tree)
				   base)
			   (funcall rec tree
						#'(lambda ()
							(self (car tree)))
						#'(lambda ()
							(if (cdr tree)
								(self (cdr tree))))))))
	#'self))

(defun rfind-if-1 (predicate tree)
  "使用`trec'生成的rfind-if"
  (funcall (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
				 #'(lambda (tree) (and (funcall predicate tree) tree)))
		   tree))

(defun rfind-if (predicate tree)
  "在`tree'中查找符合条件的第一个节点"
  (cond ((atom tree)
		 (when (funcall predicate tree)
		   tree))
		((listp tree)
		 (let ((result (rfind-if predicate (car tree))))
		   (if result
			   result
			 (rfind-if predicate (cdr tree)))))))

;; 惰性求值
(defconst unforced (gensym)
  "delay对象尚未求值的标志")

(cl-defstruct delay						;定义delay结构体,其由两个部分组成
  forced								;forced代表该delay结构体是否已经求值,若求过值,则直接使用该值
  closure)								;closure为一个闭包,调用它就能得到该delay所代表的值

(defmacro delay (expr)
  "使用该宏,将`expr'封装成一个惰性求值对象"
  (lexical-let ((self (gensym)))
	`(lexical-let ((,self (make-delay :forced unforced)))
	   (setf (delay-closure ,self)
			 #'(lambda ()
				 (setf (delay-forced ,self) ,expr)))
	   ,self)))

(defun force (x)
  "对delay对象求值
若x不是delay对象则直接返回x,否则返回其表示的值"
  (if (delay-p x)
	  (if (eq (delay-forced x) unforced)
		  (funcall (delay-closure x))
		(delay-forced x))
	x))

(defun thread-last-help-fn (&rest forms)
  (if (= 1 (length forms))
      forms
    (append (car forms)
            (apply #'thread-last-help-fn (cdr forms)))))

(defmacro ->> (&rest forms)
  (let ((forms (reverse forms)))
    (apply #'thread-last-help-fn forms)))
;; (macroexpand '(->> list-of-emps
;;                 (filter #'fn)
;;                 (map #'mfn)))

(defun expose (function)
  "Return an interactive version of FUNCTION."
  (lambda ()
    (interactive)
    (funcall function)))

(defun graphic-p ()
  "判断当前环境是否为图像环境"
  (getenv "DISPLAY"))

(provide 'elisp-helper)
