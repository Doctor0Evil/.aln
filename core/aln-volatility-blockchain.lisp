
;; /core/aln-volatility-blockchain.lisp

(defun checkpoint-volatile-logic ()
  (let ((volatile-modules (scan-aln-logic-modules)))
    (dolist (m (filter 'volatile-logic? volatile-modules))
      (let ((hash (sha256sum (read-file-content m)))
            (timestamp (iso8601-now)))
        (append-to-file "logs/blockchain-stabilization.log"
                        (format nil "~A ~A ~A~%" m hash timestamp))
        (console-log (format nil "Committed ~A to blockchain checkpoint with hash: ~A at ~A" m hash timestamp))))))
