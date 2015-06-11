(defun drawing-test ()
  (interactive)
  (insert (draw-graph graph-json)))

(defvar graphy-mode-hook nil)

(defvar graphy-mode-map
  (let ((graphy-mode-map (make-keymap)))
    (define-key graphy-mode-map "\C-j" 'newline-and-indent)
    graphy-mode-map)
  "Keymap for Graphy major mode")

;; (add-to-list 'auto-mode-alist '("\\*Kibana*\\'" . graphy-mode))

(defvar graphy-bar-face 'graphy-bar-face)
(defface graphy-bar-face
  '((t :background "#7700ff" :foreground "#7700ff"))
  "" :group 'font-lock-faces)

(defvar graphy-bar-background-face 'graphy-bar-background-face)
(defface graphy-bar-background-face
  '((t :background "#1d1f21" :foreground "#1d1f21"))
  "" :group 'font-lock-faces)

(setq graphy-font-lock
      '(("π" . graphy-bar-face)
        ("√" . graphy-bar-background-face)))

(defun graphy-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (setq font-lock-defaults '(graphy-font-lock))
  (use-local-map graphy-mode-map)
  (setq major-mode 'graphy-mode)
  (setq mode-name "Graphy")
  (run-hooks 'graphy-mode-hook))

(defun lookup (key lat)
  (cdr (assoc key lat)))

(defun kibana-buckets (json-string)
  (let ((json (json-read-from-string json-string)))
    (lookup 'buckets (lookup '\2 (lookup 'aggregations json)))))

(defun maxl (lat)
  (reduce #'max lat))

(defun doc-counts (buckets)
  (mapcar (lambda (d) (lookup 'doc_count d)) buckets))

(defun max-graph-y (buckets)
  (maxl (doc-counts buckets)))

(defvar y-len 10)

(defun graph-y-points (graph)
  (let ((lat (make-list y-len 0))
        (inc (/ (max-graph-y graph) (- y-len 1))))
    (reduce (lambda (a b)
              (cond
               ((listp a) (cons (+ inc b (car a)) a))
               (t (cons (+ inc b) (list a))))) lat)))

(defun draw-numbering (y-points)
    (mapc (lambda (y)
            (beginning-of-line)
            (insert (format "%3d |" y))
            (end-of-line)
            (newline)
            (insert (format "%3s |" ""))
            (end-of-line)
            (newline)) y-points))

(defun draw-graph (graph-json)
  (with-temp-buffer
    (let* ((buckets (kibana-buckets graph-json))
           (max (max-graph-y buckets))
           (y-points (graph-y-points buckets)))
      (draw-numbering y-points)
      (beginning-of-buffer)
      (mapcar (lambda (count) (draw-row max count)) (doc-counts (kibana-buckets graph-json)))
      (buffer-string))))

(defun draw-row (max doc-count)
  (let* ((index (- (* y-len 2) 1))
         (divided (/ max 19)))
    (delete-trailing-whitespace)
    (beginning-of-buffer)
    (while (> index 0)
      (end-of-line)
      (insert "√")
      (cond
       ((>= doc-count (* index divided)) (insert "ππ"))
       (t (insert "√√")))
      (next-line)
      (setq index (- index 1)))
    (buffer-string)))

(setq graph-json (with-temp-buffer
                  (insert-file-contents "json-example")
                  (buffer-string)))
