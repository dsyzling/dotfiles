;;; ds-ivy.el --- Personal Ivy Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: ivy, swiper, counsel

;;; Commentary:

;;; Code:

;; Use counsel/swiper with ivy
(use-package counsel :ensure t)

;; ;; Use counsel with projectile - define projectile prefix key.
(use-package counsel-projectile :ensure t
  :config
  (counsel-projectile-mode)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy)
        )
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "%d/%d")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))
        ;; '((t   . ivy--regex-fuzzy))
         ))

;; When using ivy/counsel 
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)

(use-package lsp-ivy :ensure t)

;; (use-package ivy-rich
;;   :after ivy
;;   :ensure t

;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; Use ivy-avy to trigger lens actions in lsp-mode
;; (use-package ivy-avy :ensure t)

(provide 'ds-ivy)

;;; ds-ivy.el ends here
