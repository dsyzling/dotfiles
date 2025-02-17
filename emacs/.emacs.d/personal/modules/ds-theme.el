;;; ds-theme.el --- Emacs Theme
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: Emacs, theme

;;; Commentary:

;;; Personal Emacs theme

;;; Code:

;;
;;
;;

;;
;; Colour theme catpuccin
;;
;;
;; https://github.com/catppuccin/emacs?tab=readme-ov-file
;; to check which face is active put cursor over text
;; and M-x customize-face to see the name.
;;
(use-package catppuccin-theme
  :config
  ;; or 'latte, 'macchiato, or 'mocha
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t)
  (custom-theme-set-faces
   'catppuccin
   ;; DS - these are causing issues with company-box
   ;; by default the theme uses the same face as for comments, which is wrong IMO
   ;; '(font-lock-doc-face ((t (:foreground (catppuccin-color 'green)))))
   ;; ;; font-lock variable definitions like function definitions
   ;; '(font-lock-variable-face ((t (:inherit font-lock-function-face))))
   ))

(custom-set-faces
 ;; dark-ish green.
 ;; '(org-scheduled-today ((t (:foreground "#93C572" :weight bold))))

 ;; darker peach.
 ;; '(org-scheduled-today ((t (:foreground "#ff8b3f" :weight bold))))
 ;; mocha's peach. 
 '(org-scheduled-today ((t (:foreground "#fab387" :weight bold))))
 
 ;; '(org-scheduled-today ((t (:foreground "#f6a192" :weight bold))))
 )

;;
;; Twilight
;;

;; for now let's use the default theme with a different background colour
;;(set-background-color "#211e1e")

;; (use-package twilight-theme
;;   :ensure t
;;   :config
;;   (load-theme 'twilight t))

;; ;; make comment italic and standard foreground text less 'bright'
;; ;; for the twilight theme
;; (custom-theme-set-faces
;;  'twilight
;;  '(default ((t (:background "#141414" :foreground "#cacaca"))))
;;  '(font-lock-comment-face ((t (:italic t :foreground "#5F5A60"))))
;;  '(font-lock-warning-face ((t (:background "#141414" :foreground "red"))))
;;  )
;; (set-background-color "#1E1E1E")

;; ;; Company colour theme customisation
;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))



(provide 'ds-theme)

;;; ds-python.el ends here
