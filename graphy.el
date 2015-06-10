(defun drawing-test ()
  (interactive)
  (insert (draw-graph graph-json))
  (get-points)
  (fill-points))

(defun get-points ()
  (setq draw-list '())
  (setq del-draw-list '())
  (let ((p1 1))
    (while (< p1 (point-max))
      (graphy--maybe-add-to-draw-list p1)
      (setq p1 (+ p1 1)))))

(defun fill-points ()
  (remove-overlays)
  (mapc (lambda (p)
          (overlay-put (make-overlay p (+ p 1)) 'face 'graphy-bar-face)) draw-list)
  (mapc (lambda (p)
          (overlay-put (make-overlay p (+ p 1)) 'face 'graphy-bar-background-face)) del-draw-list))

(defface graphy-bar-face
  '((t :background "#7700ff" :foreground "#7700ff"))
  "" :group 'graphy)

(defface graphy-bar-background-face
  '((t :background "#1d1f21" :foreground "#1d1f21"))
  "" :group 'graphy)

(defun graphy--maybe-add-to-draw-list (p1)
  (cond
   ((equal (buffer-substring-no-properties p1 (+ p1 1)) "@")
    (add-to-list 'draw-list p1))
   ((equal (buffer-substring-no-properties p1 (+ p1 1)) "#")
    (add-to-list 'del-draw-list p1))))

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
    (end-of-line)
    (beginning-of-buffer)
    (while (> index 0)
      (end-of-line)
      (just-one-space 1)
      (cond
       ((>= doc-count (* index divided)) (insert "@@"))
       (t (insert "##")))
      (next-line)
      (setq index (- index 1)))
    (buffer-string)))

(setq graph-json (with-temp-buffer
                  (insert-file-contents "json-example")
                  (buffer-string)))
