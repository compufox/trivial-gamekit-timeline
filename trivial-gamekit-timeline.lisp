;;;; trivial-gamekit-timeline.lisp

(defpackage #:trivial-gamekit-timeline
  (:use #:cl)
  (:nicknames :gamekit.timeline)
  (:export :timeline :keyframe

           :play-timeline :stop-timeline :pause-timeline
           :restart-timeline :frame-advance :loop-timeline
           :playingp :stoppedp :loopingp
           
           :make-keyframe))
           

(in-package #:trivial-gamekit-timeline)

(defvar *playing-timelines* nil)

(defclass timeline ()
  ((frames :initform nil)
   (loop :initarg :loop :initform nil
         :accessor loop-timeline)
   (current-frame :initform 0)
   (playing :initform nil)))

(defclass keyframe ()
  ((frame :initform nil :initarg :frame
          :reader keyframe-frame)
   (slot :initform nil :initarg :slot)
   (object :initform nil :initarg :object
           :reader keyframe-object)
   (event :initform nil :initarg :event)
   (target :initform nil :initarg :target)
   (process-fn :initform nil :initarg :process-fn)))

(defun play-timeline (timeline)
  "play TIMELINE"
  (setf (slot-value timeline 'playing) t)
  (push timeline *playing-timelines*))

(defun pause-timeline (timeline)
  "pause TIMELINE"
  (setf *playing-timelines*
        (remove timeline *playing-timelines* :test #'eql)))

(defun stop-timeline (timeline)
  "stop TIMELINE"
  (setf (slot-value timeline 'playing) nil)
  (setf *playing-timelines* (remove timeline *playing-timelines*)))

(defun restart-timeline (timeline)
  "restart TIMELINE"
  (setf (slot-value timeline 'current-frame) 0))

(defun playingp (timeline)
  "returns whether or not a timeline is currently playing"
  (slot-value timeline 'playing))

(defun stoppedp (timeline)
  "returns whether or not a timeline is currently stopped"
  (not (slot-value timeline 'playing)))

(defun loopingp (timeline)
  "returns whether or not a timeline is set to loop"
  (slot-value timeline 'loop))

(defun frame-advance (timeline &optional (amount 1))
  "advance TIMELINE by AMOUNT of frames

AMOUNT defaults to 1"
  (incf (slot-value timeline 'current-frame) amount))

(defun make-keyframe (timeline frame &key object slot event target (process-fn #'gamekit:lerp))
  "creates a keyframe at FRAME and adds it into TIMELINE

OBJECT and SLOT must be passed in together.
OBJECT is any standard lisp object
SLOT is a symbol representing a slot in OBJECT
EVENT is a function that accepts no arguments
TARGET is the target value that the SLOT of OBJECT should be by FRAME
PROCESS-FN is the function that returns the new value to be set in SLOT of OBJECT, defaults to gamekit:lerp"
  (let ((kf (make-instance 'keyframe :frame frame :event event :target target :process-fn process-fn)))
    (when (and object slot)
      (setf (slot-value kf 'object) object
            (slot-value kf 'slot) slot))
    (with-slots (frames) timeline
      (push kf frames)
      (setf frames (sort frames #'< :key #'keyframe-frame)))))

(defun process-timelines ()
  "goes through and processes the next keyframe for any playing timeline"
  (dolist (tl *playing-timelines*)
    (with-slots (current-frame frames) tl
      (let ((keyframes (member (find current-frame frames :test #'<= :key #'keyframe-frame)
                               frames)))
        (if keyframes
            (loop for key in (collect-uniques keyframes :key #'keyframe-object)
                  do (let ((object (slot-value key 'object))
                           (slot (slot-value key 'slot))
                           (target (slot-value key 'target))
                           (event (slot-value key 'event))
                           (process-fn (slot-value key 'process-fn))
                           (frame (slot-value key 'frame)))
                       (cond
                         (object
                          (setf (slot-value object slot)
                                (funcall process-fn (slot-value object slot) target
                                         ;; divides the percent by 2 because for some reason
                                         ;; lerps happen at twice the rate
                                         (if (zerop frame) 0 (/ (/ current-frame frame) 2)))))
                         ((and (= current-frame frame) event)
                          (funcall event)))))
            (if (loopingp tl)
                (setf current-frame 0)
                (stop-timeline tl))))
      (incf current-frame))))

(defun collect-uniques (l &key (key #'identity) (test #'eql))
  (loop for v in l
        
        unless (member (funcall key v) results :test test :key key)
          collect v into results

        finally (return results)))

;; ensures that we process our timelines before each frame
(defmethod gamekit:act :before ((this gamekit:gamekit-system))
  (process-timelines))
