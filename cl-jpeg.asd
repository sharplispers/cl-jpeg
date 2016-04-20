(asdf:defsystem :cl-jpeg
  :name "cl-jpeg"
  :version "1.9"
  :license "BSD"
  :description "A self-contained baseline JPEG codec implementation"
  :author "Eugene Zaikonnikov; contributions by Kenan Bölükbaşı, Manuel Giraud, Cyrus Harmon and William Halliburton"
  :components ((:file "package")
	       (:file "jpeg" :depends-on ("package"))
	       (:file "io" :depends-on ("jpeg"))))
