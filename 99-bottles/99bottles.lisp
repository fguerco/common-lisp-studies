;; simple

(defun bottles (bot &optional (output t))
  (format output "~%~@(~r~) bottle~:p of beer on the wall~%" bot)
  (format output "~@(~r~) bottle~:p of beer~%" bot)
  (format output "Take one down, pass it around~%")
  (format output "~@(~r~) bottle~:p of beer~%" (1- bot))
  (when (> bot 1) (bottles (1- bot) output)))
      

(bottles 99)
