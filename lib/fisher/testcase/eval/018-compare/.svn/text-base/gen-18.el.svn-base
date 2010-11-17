(setq gen18-L ["null" "1" "2" "'a'" "'b'"])
(setq gen18-C 
      '(
        ("<=" <=)
        ("<"  <)
        (">=" >=)
        (">"  >)
        ("==" =)
        ("!=" /=)
        ))

(defun gen18b() (interactive)
  ;; Arrange L so that the indices are in the same order
  ;; as the values.  Then C will compare indices,
  ;; not values.
  (let ((L gen18-L)
        (C gen18-C))
    (insert "{\n")
    (for i 0 (- (length L) 1)
      (for j 0 (- (length L) 1)
        (dolist (c C)
          (let* ((vi (aref L i))
                 (vj (aref L j))
                 (op (car c))
                 (lo (cadr c)) ; lisp operator name
                 (res (funcall lo i j))
                 (re (if res "true" "false"))
                 )
            (insert-format
             "\n  ~!@eq(%s %s %s, %s)@!~;"
                        vi op vj  re
                        )
            );let vi vj op lo res
          ))); dolist for for
    (insert "\n}\n")
    )
  ); defun


(defun gen18c() (interactive)
  ;; Arrange L so that the indices are in the same order
  ;; as the values.  Then C will compare indices,
  ;; not values.
  (let ((L gen18-L)
        (C gen18-C))
    (insert "{\n")
    (for i 0 (- (length L) 1)
      (for j 0 (- (length L) 1)
        (for k 0 (- (length L) 1)
          (dolist (c C)
            (dolist (d C)
              (let* ((vi (aref L i))
                     (vj (aref L j))
                     (vk (aref L k))
                     (o1 (car c))
                     (l1 (cadr c)) ; lisp operator name
                     (o2 (car d))
                     (l2 (cadr d))
                     (res (and (funcall l1 i j) (funcall l2 j k)))
                     (re (if res "true" "false"))
                     )
            (insert-format
             "\n  ~!@eq(%s %s %s %s %s, %s)@!~;"
                        vi o1 vj o2 vk  re
                        )
            );let vi vj op lo res
          ))))); dolist for for
    (insert "\n}\n")
    )
  ); defun

