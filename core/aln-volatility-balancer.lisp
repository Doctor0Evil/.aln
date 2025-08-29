;; /core/aln-volatility-balancer.lisp

(defun balance-repo-volatility ()
  ;; Scan all logic modules for random/volatile logic, ensure overlays exist for hotpatch & compliance.
  (let* ((modules (scan-aln-logic-modules))
         (volatile (filter 'volatile-logic? modules)))
    (dolist (m volatile)
      (unless (has-balance-overlay? m)
        (add-hotpatch-overlay m)))
    (console-log (format nil "Volatility balancing complete, overlays ensured for: ~A" volatile))))
