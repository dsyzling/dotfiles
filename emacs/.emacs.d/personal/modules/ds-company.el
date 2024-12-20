;;; ds-company.el --- Personal company Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: company
;; See:
;;

;;; Commentary:

;;; Code:

;;

;;
;; Company mode customisation.
;;
(use-package company
  :ensure t
  :config
  ;; Reduce company idle delay for completion
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 50
        company-icon-size 20)
  :init
  (global-company-mode 1)
  ;; In case we want to override completion style -
  ;; Default is basic partial-completion
  ;;(setq completion-styles '(partial-completion))
  ;;(add-to-list 'company-backends '(company-capf :with company-yasnippet))

  ;; override idle delay for certain modes, otherwise it can be annoying
  ;; when typing.
  :hook
  (org-mode . (lambda ()
                (set (make-local-variable 'company-idle-delay) 0.4)))
  )

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Company colour theme customisation
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:background ,(color-lighten-name bg 10)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;;
;; company-quickhelp is not displaying the popup correctly on mac osx or
;; Ubuntu WSL.
;; Have reverted to company-box until these issues are resolved.
;;
;; (use-package company-quickhelp
;;   :ensure t
;;   :config
;;   (company-quickhelp-mode)
;;   :custom
;;   (company-quickhelp-use-propertized-text t)
;;   (company-quickhelp-max-lines 20))

;; ;; Set colours for company quick-help
;; (setq company-quickhelp-color-background "#233c233c233c")
;; (setq company-quickhelp-color-foreground "#F8F8F8")

(provide 'ds-company)

;;; ds-company.el ends here
