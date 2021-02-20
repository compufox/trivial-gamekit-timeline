(defpackage :trivial-gamekit-timeline-example
  (:nicknames :gamekit.timeline-example)
  (:use :cl :gamekit.timeline :gamekit)
  (:export :run))

(in-package :trivial-gamekit-timeline-example)

(defgame example () ()
  (:viewport-title "timeline example"))
(defclass sprite ()
  ((position :initarg :pos :accessor sprite-pos)
   (color :initarg :col :accessor sprite-color)))

(defvar *sprite-timeline* (make-timeline))
(defvar *sprite1* (make-instance 'sprite :pos (vec2 0 0) :col (vec4 1 1 1 1)))
(defvar *sprite2* (make-instance 'sprite :pos (vec2 400 400) :col (vec4 0 1 0 1)))
(defvar *draw-text* nil)
(defvar *sprites* (list *sprite1* *sprite2*))

(make-keyframe *sprite-timeline* 0
               :object *sprite1* :slot 'position
               :target (vec2 0 0))
(make-keyframe *sprite-timeline* 0
               :object *sprite2* :slot 'color
               :target (vec4 0 1 0 1))
(make-keyframe *sprite-timeline* (* 60 1)
               :object *sprite1* :slot 'position
               :target (vec2 100 100))
(make-keyframe *sprite-timeline* (* 60 5)
               :object *sprite2* :slot 'color
               :target (vec4 1 0 0 1))
(make-keyframe *sprite-timeline* (* 60 10)
               :object *sprite2* :slot 'color
               :target (vec4 0 0 1 1))
(make-keyframe *sprite-timeline* (* 60 3)
               :event #'(lambda () (setf *draw-text* t)))

(defmethod gamekit:draw ((this example))
  (gamekit:draw-rect (Vec2 0 0) 800 600 :fill-paint (vec4 0 0 0 1))
  (gamekit:draw-text "space: run timeline, r: reset, escape: quit"
                     (vec2 200 20) :fill-color (Vec4 1 1 1 1))
  (when *draw-text*
    (gamekit:draw-text "My event fired!" (vec2 200 100) :fill-color (vec4 1 1 1 1)))
  (dolist (s *sprites*)
    (gamekit:draw-rect (sprite-pos s) 20 20 :fill-paint (sprite-color s))))

(defun run ()
  (gamekit:start 'example)
  (gamekit:bind-button :space :pressed
                       (lambda ()
                         (restart-timeline *sprite-timeline*)
                         (play-timeline *sprite-timeline*)))
  (gamekit:bind-button :r :pressed
                       (lambda ()
                         (stop-timeline *sprite-timeline*)
                         (setf *draw-text* nil
                               (sprite-pos *sprite1*) (vec2 0 0)
                               (sprite-color *sprite2*) (vec4 0 1 0 1))))
  (gamekit:bind-button :escape :pressed
                       (lambda ()
                         (gamekit:stop))))
