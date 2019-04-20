(load "hacksthenet.lisp")

(save-lisp-and-die "hacksthenet"
                   :toplevel #'hacksthenet:start-demo
                   :executable t)
