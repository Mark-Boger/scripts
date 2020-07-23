(in-package :cl-user)

(defpackage #:xrandr
  (:use #:cl #:inferior-shell)
  (:import-from :str :split :join)
  (:export
   #:reset-screens))

(in-package :xrandr)

(defvar *dpi* 196
  "Assuming 4k resolution on at least one of the screens")

(defun set-dpi (dpi)
  (setf *dpi* dpi))

(defun get-connected-screens ()
  (run/lines `(pipe (xrandr) (grep " connected") (awk "{ print $1 }"))))

(defun get-preferred-resolution ()
  ;; I'm not sure why this regex is working
  (run/lines `(pipe (xrandr) (egrep "[*]+") (awk "{ print $1 }"))))

(defun get-scales ()
  (let* ((resolutions (loop :for res :in (get-preferred-resolution)
                            :collect (mapcar #'parse-integer (split "x" res))))
         ;; Finding the max res could probably be better
         ;; Find the max w and assume that it's the part of the largest resolution
         (max-w (eval `(max ,@(loop :for (w h) :in resolutions :collect w))))
         ;; Using max-w find the full resolution
         (max-res (find max-w resolutions :key #'first)))
    ;; Make the format that xrandr can understand (2x2)
    (mapcar (lambda (s)
              (if (every (lambda (x) (= 1 x)) s)
                  ;; These screens have a scale of 1x1 so mark them with a * for later
                  (list (join "x" s) "*")
                  (list (join "x" s) "+"))) 
            (loop :for res :in resolutions :collect (mapcar #'/ max-res res)))))

(defun get-screens ()
  ;; All of these produce parallel lists so just combine them
  (let ((s (mapcar 'list (get-connected-screens) (get-preferred-resolution))))
    (mapcar #'append s (get-scales))))

(defun reset-screens ()
  (loop :for (screen res scale) :in (get-screens)
        ;; Make sure the scale is 1x1 and the resolution is the preferred one
        :do (run `(xrandr --output ,screen --scale 1x1 --mode ,res))
            ;; Make sure the panning is the preferred one
            (run `(xrandr --output ,screen --panning ,res))))

(defun right ()
  (let* ((screens (get-screens))
         (main-screen (find "*" screens :key (lambda (x) (car (last x)))
                                        :test #'string=))
         (ps (mapcar #'parse-integer (split "x" (second main-screen))))
         (fb (join "x" (list (* (length screens) (first ps)) (second ps)))))
    (run
     `(xrandr
       --dpi ,*dpi* --fb ,fb
       --output ,(first main-screen) --mode ,(second main-screen)
       --pos ,(join "x" (list (first ps) 0))
       ,@(loop :for (screen res scale _) :in screens
               :for idx := 1 :then (1+ idx)
               :unless (string= screen (first main-screen))
                 ;; This isn't great
                 :append `(--output ,screen --scale ,scale
                                    --panning
                                    ,(join "+" (list (second main-screen)
                                                     (* idx (first ps))
                                                     0))))))))
