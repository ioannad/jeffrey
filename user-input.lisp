#|
# user-input package

To sanitize, handle, and transform user input
|#

(defun handle-non-integer-input (input function)
  (handler-case (parse-integer input)
    (parse-integer-not-integer-string (e)
      (format *standard-output* "Your entry, ~a, is not an integer" e)
      (function))))

