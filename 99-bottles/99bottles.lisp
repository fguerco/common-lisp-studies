
(defun bottles (&optional (bot 99) (output t))
  (when (> bot 0)
    (progn
      (format output "~%~@(~r~) bottle~:p of beer on the wall~%" bot)
      (format output "~@(~r~) bottle~:p of beer~%" bot)
      (format output "Take one down, pass it around~%")
      (format output "~@(~[no more~:;~:*~r~]~) bottle~:p of beer~%" (1- bot))
      (bottles (1- bot) output))))
      
(bottles)
