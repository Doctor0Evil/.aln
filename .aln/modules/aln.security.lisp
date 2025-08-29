;;; ========================== ALN FULL SECURITY + DEVOPS MODULE (LISP LOGIC) ============================
;; Core: Auth (JWT+OAuth2), Automation, K8s RBAC/netpolicy
(defpackage :aln.security
  (:use :cl :aln)
  (:export :auth-init :jwt-login :jwt-validate :secure-data-access :pytest-validate :github-actions-pipeline :k8s-rbac-setup :k8s-netpol-setup))

(in-package :aln.security)

;;;; ------------------- Auth Logic -------------------

(defvar *secret-key* "supersecretjwtkey") ; TODO: swap to ENV in prod
(defvar *jwt-algorithm* "HS256")
(defvar *token-expires-mins* 30)

(defun jwt-login (username password)
  (if (and (string= username "admin") (string= password "secret"))
      (create-jwt-token `(("sub" . ,username)))
      (error "AUTH_FAIL"))
  ) ; @github-file-destination:src/api/auth.py

(defun create-jwt-token (claims)
  (let* ((exp (aln:utcnow+minutes *token-expires-mins*))
         (payload (append claims `(("exp" . ,exp)))))
    (aln:jose/jwt-encode payload *secret-key* *jwt-algorithm*))
  ) ; @github-file-destination:src/api/auth.py

(defun jwt-validate (token)
  (handler-case
      (let ((payload (aln:jose/jwt-decode token *secret-key* *jwt-algorithm*)))
        (or (getf payload "sub") (error "INVALID_TOKEN")))
    (error (e) (error "TOKEN_EXPIRED/INVALID")))
  ) ; @github-file-destination:src/api/auth.py

(defun secure-data-access (token)
  (let ((user (jwt-validate token)))
    (format nil "Hello, ~A!" user))
  ) ; @github-file-destination:src/api/auth.py

;;;; ------------------- Pytest Automation -------------------

(defun pytest-validate ()
  (aln:sh "pytest -v --maxfail=1 --disable-warnings tests/test_auth.py"))

;;;; ------------------- GitHub Actions Pipeline -------------------

(defun github-actions-pipeline ()
  "Simulate workflow for auth tests on CI."
  (aln:github-actions
   :workflow ".github/workflows/tests.yml"
   :steps '("checkout"
            "setup-python3.11"
            "install: fastapi uvicorn pytest jose python-multipart"
            "pytest")))

;;;; ------------------- Kubernetes RBAC & NetworkPolicy -------------------

(defun k8s-rbac-setup ()
  (aln:k8s/apply-yaml "k8s/rbac.yaml"))

(defun k8s-netpol-setup ()
  (aln:k8s/apply-yaml "k8s/networkpolicy.yaml"))

;;; ========================================================================================================
