;; TODO: (Rasheed's & Hamza's suggestions)
;; + Executable
;; + Images
;; + Multiplayer (Battle Mode, Soccer Mode, (Totally not coppying STK))
;; + Change Theme (colors, bg, snake img, apple img)
;; + Noises/Sounds (death, eat, turn, new-game)
;; + Random Portals
;; + Dimensions/Levels (on 100 score)
;; + Secret levels (person has to guess key combination with hints)
;; + Gaining and Stealing items (apples, coins)
;; + Weapons (Sword, Bazooka, Grenade)
;; + Bite and cut snake in half (steal other parts)
;; + Snake's and Ladder's, menu game 1-100 levels like the board
;; + options have to show up
;; + Tounge attack which pulls opponent snake
;; + Online multiplayer (WIGs)

(defpackage mcnibbles
  (:use :clim-lisp :clim))
(in-package :mcnibbles)

(defparameter *board-width* 40)
(defparameter *board-height* 40)
(defparameter *y-off* 20) ;; for menubar
(defparameter *sleep-time* 0.1)
(defvar *running* nil)

;;; Generics
(defgeneric snake-head (snake))
(defgeneric (setf snake-head) (newvalue snake))
(defgeneric collision? (snake))
(defgeneric collision-self (snake &optional index))
(defgeneric collision-wall (snake))
(defgeneric advance-snake (snake))
(defgeneric snake-next-pos (snake))
(defgeneric set-dir (snake dir))
(defgeneric apple-eaten? (snake apple))
(defgeneric make-apple (snake))
(defgeneric make-apple-pos (snake apple &optional pos))

;;; Interface between Objects and GUI
(defgeneric draw-snake (snake pane))
(defgeneric draw-apple (apple pane))

(defstruct pos
  (x 20 :type integer)
  (y 20 :type integer))

;;; Snake Class
(defclass snake ()
  ((cells :initform (make-array 1
                      :initial-contents (list (make-pos))
                      :adjustable t :fill-pointer t)
          :accessor snake-cells)
   (direction :initform 'up :accessor snake-dir)
   (state :initform 'alive :accessor snake-state)
   (growth-points :initform 0 :accessor snake-growth-points)))

(defun make-snake ()
  (make-instance 'snake))

(defmethod set-dir ((s snake) dir)
  (assert (or (eq dir 'up)
              (eq dir 'down)
              (eq dir 'left)
              (eq dir 'right)))

  (let ((s-dir (snake-dir s)))
    ;; Unless trying to set opposite direction
    ;; AND The snake is more than just a head, Change Direction
    (unless (and (or (and (eql dir 'up)    (eql s-dir 'down))
                     (and (eql dir 'down)  (eql s-dir 'up))
                     (and (eql dir 'left)  (eql s-dir 'right))
                     (and (eql dir 'right) (eql s-dir 'left)))
              (/= (length (snake-cells s)) 1))
      (setf (snake-dir s) dir))))

(defmethod snake-head ((s snake))
  (aref (snake-cells s) 0))

(defmethod (setf snake-head) (newvalue (s snake))
  (setf (aref (snake-cells s) 0) newvalue))

(defmethod snake-next-pos ((s snake))
  (let* ((d (snake-dir s))
         (p (snake-head s))
         (x (pos-x p))
         (y (pos-y p)))

    (case d
      (up (decf y))
      (down (incf y))
      (left (decf x))
      (right (incf x)))

    (make-pos :x x :y y)))

(defmethod advance-snake ((s snake))
  (let ((snake-len (length (snake-cells s))))
    (when (> (snake-growth-points s) 0)
      (vector-push-extend (make-pos) (snake-cells s)
                          (snake-growth-points s))
      (incf snake-len)
      (decf (snake-growth-points s)))

    (loop :for i :from (1- snake-len) :downto 1
      :do (setf (aref (snake-cells s) i)
            (aref (snake-cells s) (1- i))))

    (setf (snake-head s) (snake-next-pos s)))

  (when (collision? s)
    (setf (snake-state s) 'dead)))

;; Recursively check if any cell is at the same position
;; as the snake's head.
(defmethod collision-self ((s snake) &optional (index 1))
  (cond
    ((eql index (length (snake-cells s))) nil)
    ((equalp (snake-head s) (aref (snake-cells s) index)) t)
    (t (collision-self s (+ index 1)))))

(defmethod collision-wall ((s snake))
  (let* ((p (snake-head s))
         (x (pos-x p))
         (y (pos-y p)))
    (cond
      ((< x 0) t)
      ((< y 0) t)
      ((>= x *board-width*) t)
      ((>= y *board-height*) t)
      (t nil))))

(defmethod collision? ((s snake))
  (or (collision-self s) (collision-wall s)))

;;; Apple Code
(defclass apple ()
  ((place :initform (make-pos
                      :x (random *board-width*)
                      :y (random *board-height*))
     :accessor apple-pos)))

(defmethod make-apple ((s snake))
  (let ((a (make-instance 'apple)))
    (setf (apple-pos a) (make-apple-pos s a))
    a))

(defmethod make-apple-pos ((s snake) (a apple) &optional
                            (pos
                              (make-pos
                                :x (random *board-width*)
                                :y (random *board-height*))))
  (let ((cells (snake-cells s)))

    ;; Too simple; Can spawn apple where a snake cell is
    ;; (setf pos (make-pos :x (random *board-width*)
    ;;                     :y (random *board-height*)))

    ;; Loop across all snake cells
    ;; If Apple's position is the same as any Snake cell position,
    ;; Choose another random position and LOOP to check
    (loop :for cell :across cells
      :do (when (equalp pos cell)
            (setf pos (make-pos :x (random *board-width*)
                        :y (random *board-height*)))
            (make-apple-pos s a pos)))

    pos))

(defmethod apple-eaten? ((s snake) (a apple))
  (equalp (snake-head s) (apple-pos a)))

;;; GUI + Objects

(defmethod draw-snake ((s snake) pane)
  ;; (map nil
  ;;   #'(lambda (p)
  ;;       (plot pane (pos-x p) (pos-y p) 10 10 :y-off *y-off*
  ;;         :color +green+))
  ;;   (snake-cells s))

  (loop :for cell :across (snake-cells s)
        :for a :from 0 :do
    (if (zerop a)
      (plot pane (pos-x cell) (pos-y cell) 10 10
        :y-off *y-off* :color +green3+)
      (plot pane (pos-x cell) (pos-y cell) 10 10
        :y-off *y-off* :color +dark-green+))))

(defmethod draw-apple ((a apple) pane)
  (plotc pane (pos-x (apple-pos a))
              (pos-y (apple-pos a))
    5 :y-off *y-off* :color +red+))

;;; GUI
(defclass board-pane (clim-stream-pane)
  ())

(define-application-frame snake-app ()
  ((snake :initform nil :accessor app-snake)
   (apple :initform nil :accessor app-apple)
   (score :initform 0   :accessor app-score))

  (:menu-bar snake-menu-bar)

  (:panes
    (app (make-pane 'board-pane
           :display-time t
           :display-function 'display-app
           ;; :double-buffering t
           :height (+ (* *board-height* 10) *y-off*)
           :width (* *board-width* 10))))

  (:layouts
    (default app)))

(defun get-app-pane (&optional (frame *application-frame*))
  (find-pane-named frame 'app))

;; (defun plot (pane x y width height &key (color +black+))
;;   "Plot a 10x10 'pixel' in the application frame"
;;   (draw-rectangle* pane
;;     (* x width) (* y height)
;;     (+ (* x width) width) (+ (* y height) height)
;;     :ink color))

;; (defun plotc (pane x y radius &key (color +black+))
;;   "Plot a circle in the application frame"
;;   (draw-circle* pane
;;     (+ (* x (* radius 2)) radius)
;;     (+ (* y (* radius 2)) radius)
;;     radius :ink color))

(defun plot (pane x y width height &key (color +black+)
                                        (x-off 0) (y-off 0))
  "Plot a 10x10 'pixel' in the application frame"
  (draw-rectangle* pane
    (+ (* x width) x-off)
    (+ (* y height) y-off)
    (+ (* x width) width x-off)
    (+ (* y height) height y-off)
    :ink color))

(defun plotc (pane x y radius &key (color +black+)
                                   (x-off 0) (y-off 0))
  "Plot a circle in the application frame"
  (draw-circle* pane
    (+ (* x (* radius 2)) radius x-off)
    (+ (* y (* radius 2)) radius y-off)
    radius :ink color))

(defun play-snake (frame)
  (queue-event (frame-top-level-sheet frame)
               (make-instance 'refresh-event :sheet frame))

  (loop while (eq (snake-state (app-snake frame))
                'alive) :do
    (progn
      (sleep *sleep-time*)
      (advance-snake (app-snake frame))
      (when (apple-eaten? (app-snake frame) (app-apple frame))
        (incf (app-score frame) 10)
        (setf (app-apple frame)
              (make-apple (app-snake frame)))
        (incf (snake-growth-points (app-snake frame)) 1))

      (queue-event (frame-top-level-sheet frame)
        (make-instance 'refresh-event
          :sheet frame)))))


;; (window-clear pane)
;; (plot pane 0 0 10 10 :y-off *y-off*)
;; (plotc pane 1 1 5 :y-off *y-off*)

(defun draw-board (pane snake apple score)
  (window-clear pane)
  (cond ((eq (snake-state snake) 'dead)
         (draw-text* pane "Your snake is dead!"
                     (- (* *board-width* 5) 40)
                     (- (* *board-height* 5) 6))
         (draw-text* pane (format nil "Score: ~d" score)
                     (- (* *board-width* 5) 40)
                     (+ (* *board-height* 5) 6)))
        (t
          (draw-rectangle* pane 0 0 (* *board-width* 10) 20 :ink +black+)
          (draw-text* pane (format nil "Score: ~d" score) 5 15
            :ink +white+)
          (draw-snake snake pane)
          (draw-apple apple pane))))

(defclass refresh-event (window-manager-event) ())

(defmethod handle-event ((frame snake-app) (event refresh-event))
  (draw-board (get-app-pane frame)
              (app-snake frame)
              (app-apple frame)
              (app-score frame)))

(defmethod handle-event ((pane board-pane) (event key-press-event))
  (let ((s (app-snake *application-frame*)))
    (unless (null s)
      (case (keyboard-event-key-name event)
        (:up (set-dir s 'up))
        (:down (set-dir s 'down))
        (:left (set-dir s 'left))
        (:right (set-dir s 'right)))))
  (call-next-method))

(defun display-app (frame stream)
  (declare (ignore frame stream))
  (let ((pane (get-app-pane)))
    (unless (null (app-snake *application-frame*))
      (draw-board pane
                  (app-snake *application-frame*)
                  (app-apple *application-frame*)
                  (app-score *application-frame*)))))

(make-command-table 'snake-menu-bar
                    :errorp nil
                    :menu '(("Game" :menu snake-game-menu)))

(make-command-table 'snake-game-menu
                    :errorp nil
                    :menu '(("New Game" :command com-new)
                            ("Quit" :command com-quit)))

;; TODO:
;; Unwind protect *running* to nil
(define-snake-app-command (com-new :name t) ()
  (let ((frame *application-frame*))
    (unless *running*
      (clim-sys:make-process
        #'(lambda ()
            (setf *running* t)
            (setf (app-snake frame) (make-snake))
            (setf (app-apple frame)
                  (make-apple (app-snake frame)))
            (setf (app-score frame) 0)
            (play-snake frame)
            (setf *running* nil))))))

(define-snake-app-command (com-quit :name t) ()
  (setf *running* nil)
  (frame-exit *application-frame*))

(defun mcnibbles ()
  (run-frame-top-level (make-application-frame 'snake-app)))

(defun mcnibbles-repl ()
  (find-application-frame 'snake-app))
