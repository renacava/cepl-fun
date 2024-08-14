;;;; cepl-fun.asd

(asdf:defsystem #:cepl-fun
  :description ""
  :author "renacava"
  :license  "pending"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl
               #:cepl.sdl2
               #:cepl.sdl2-image
               #:sdl2-ttf
               #:sdl2-image
               #:nineveh
               #:temporal-functions
               #:cepl.skitter.sdl2
               #:dirt
               #:cl-soil
               #:rtg-math
               #:sdl2-game-controller-db
               #:fiveam
               #:deploy
               #:livesupport)
  :components ((:file "package")
               (:file "main"))
	:defsystem-depends-on (:deploy)
	:build-operation "deploy-op"
	:build-pathname "cepl-fun"
	:entry-point "cepl-fun::main")
