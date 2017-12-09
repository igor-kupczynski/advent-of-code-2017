(defun == (a b) (= a b))

(defun != (a b) (/= a b))

(setq my-advent-day8-inc-dec
      #s(hash-table  data (inc (lambda (n d) (+ n d)) dec (lambda (n d) (- n d)))))

(defun my-advent-day8-get-register (state register)
  (gethash register state 0))

(defun my-advent-day8-eval-condition (state register operator operand)
  (let ((val (my-advent-day8-get-register state register)))
    (funcall operator val operand)))

(defun my-advent-day8-process-line (state line)
  (let ((parts (split-string line))
        (registers (nth 0 state))
        (max-so-far (nth 1 state)))
    (let ((register (nth 0 parts))
          (operator (gethash (intern-soft (nth 1 parts)) my-advent-day8-inc-dec))
          (operand (string-to-number (nth 2 parts)))
          (condition-register (nth 4 parts))
          (condition-operator (intern-soft (nth 5 parts)))
          (condition-operand (string-to-number (nth 6 parts))))
      (when (my-advent-day8-eval-condition registers condition-register condition-operator condition-operand)
        (puthash
         register
         (funcall operator (my-advent-day8-get-register registers register) operand)
         registers))
      (let ((highest-this-iteration (my-advent-day8-highest-value registers)))
        (list registers (max highest-this-iteration max-so-far))))))

(defun my-advent-day8-highest-value (registers)
  (let ((max-value -536870912))
    (maphash (lambda (key value)
               (when (> value max-value)
                 (setq max-value value)))
             registers)
    max-value))

(defun my-advent-day8 ()
  (interactive)
  (let ((registers (make-hash-table :test 'equal))
        (max-value -536870912)
        (lines (split-string
                (with-temp-buffer
                  (insert-file-contents-literally (read-file-name "Input: "))
                  (buffer-substring-no-properties (point-min) (point-max))) "\n" t)))
    (let ((end-state (seq-reduce 'my-advent-day8-process-line lines (list registers max-value))))
      (print (my-advent-day8-highest-value (nth 0 end-state)))
      (print (nth 1 end-state)))))

