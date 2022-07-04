;; Homepage: https://fabricate.site
;;
;; This file is not part of GNU Emacs.
(use-package polymode
  :config
  (define-hostmode poly-fabricate-hostmode
    :mode 'fundamental-mode)
  (define-hostmode poly-fabricate-md-hostmode
    :mode 'markdown-mode)
  (define-innermode poly-fabricate-clojure-innermode
    :mode 'clojure-mode
    :head-matcher "\"?✳ *[-=]?"
    :tail-matcher "🔚\"?"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-fabricate-mode
    :hostmode 'poly-fabricate-hostmode
    :innermodes '(poly-fabricate-clojure-innermode))
  (define-polymode poly-fabricate-md-mode
    :hostmode 'poly-fabricate-md-hostmode
    :innermodes '(poly-fabricate-clojure-innermode)))

