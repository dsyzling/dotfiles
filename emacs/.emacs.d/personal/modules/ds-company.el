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
(require 'python)
(use-package company
  :ensure t
  :config
  ;; Reduce company idle delay for completion
  (setq company-idle-delay 0
        company-tooltip-idle-delay 0
        ;; company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 50
        company-require-match nil
        company-icon-size 20
        )
  :init
  (global-company-mode 1)
  ;; In case we want to override completion style -
  ;; Default is basic partial-completion
  ;;(setq completion-styles '(partial-completion))
  ;;(add-to-list 'company-backends '(company-capf :with company-yasnippet))
  (setq company-backends '(company-capf))
  
  ;; override idle delay for certain modes, otherwise it can be annoying
  ;; when typing.
  :hook
  (org-mode . (lambda ()
                (set (make-local-variable 'company-idle-delay) 0.4)))
  )

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;;
;; Note any company colour customisations required have been moved
;; to ds-theme.el
;;

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
