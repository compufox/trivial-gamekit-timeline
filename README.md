# trivial-gamekit-timeline
### _ava fox_

a module for trivial-gamekit.

adds in the concept of "timelines" which keyframes can be added into, allowing for easy, programmatic animations on any object

by default interpolates between keyframe values with `gamekit:lerp` which handles `gamekit:vec2`, `gamekit:vec3`, `gamekit:vec4`, `gamekit:quat`, and `number` 

## Installation

```shell
$ git clone https://github.com/compufox/trivial-gamekit-timeline ~/common-lisp/
$ ros run # (or run whichever lisp you use)
* ;; install the quicklisp distro that houses trivial-gamekit
* (ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
* ;; ready to go!
```

## Example 

load example into lisp:
```lisp
* ;; load example
* (ql:quickload :trivial-gamekit-timeline-example)
* (gamekit.timeline-example:run)
```

source code at: `example.lisp`


## API

`(make-timeline &key loop)`

creates a timeline

if LOOP is non-nil the timeline will loop after completion

---

`(make-keyframe timeline frame &key object slot event target (process-fn #'gamekit:lerp))`

creates a keyframe at FRAME and adds it into TIMELINE


OBJECT and SLOT must be passed in together.

OBJECT is any standard lisp object

SLOT is a symbol representing a slot in OBJECT

EVENT is a function that accepts no arguments

TARGET is the target value that the SLOT of OBJECT should be by FRAME

PROCESS-FN is the function that returns the new value to be set in SLOT of OBJECT, defaults to gamekit:lerp.

PROCESS-FN should accept 3 parameters: original value, target value, percentage between. it should return the current value between the two points

---

`(play-timeline timeline)`

play TIMELINE

---

`(pause-timeline timeline)`

pause TIMELINE

---

`(stop-timeline timeline)`

stop TIMELINE

---

`(restart-timeline timeline)`

restart TIMELINE

---

`(frame-advance timeline &optional (amount 1))`

advance TIMELINE by AMOUNT of frames

AMOUNT defaults to 1

---

`(loopingp timeline)`

returns whether or not a timeline is set to loop

---

`(stoppedp timeline)`

returns whether or not a timeline is currently stopped

---

`(playingp timeline)`

returns whether or not a timeline is currently playing

---

## License

MIT

