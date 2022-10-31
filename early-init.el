;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Don't use package.el, we'll use straight.el instead
(setq package-enable-at-startup nil)
