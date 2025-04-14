;;; ds-corfu.el --- Personal Corfu Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: corfu
;; See:
;;   https://github.com/minad/corfu
;;   https://kristofferbalintona.me/posts/202202270056/;
;;
;; Consider Prescient in future:
;;  https://kristofferbalintona.me/posts/202504050923/

;;; Commentary:

;;; Code:

;;
;; Enable corfu
;;
;; A basic corfu config
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto-delay 0)
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )




;; (use-package corfu
;;   :custom
;;   ;; (:keymaps 'corfu-map
;;   ;;  :states 'insert
;;   ;;  "C-n" #'corfu-next
;;   ;;  "C-p" #'corfu-previous
;;   ;;  "<escape>" #'corfu-quit
;;   ;;  "<return>" #'corfu-insert
;;   ;;  "M-d" #'corfu-show-documentation
;;   ;;  "M-l" #'corfu-show-location)

;;   (corfu-auto nil) ; Only use `corfu' when calling `completion-at-point' or
;;                                         ; `indent-for-tab-command'
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.25)

;;   (corfu-min-width 80)
;;   (corfu-max-width corfu-min-width)     ; Always have the same width
;;   (corfu-count 14)
;;   (corfu-scroll-margin 4)
;;   (corfu-cycle nil)

;;   ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
;;   ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
;;   ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
;;   ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
;;   ;; configuration already has pre-prepared). Necessary for manual corfu usage with
;;   ;; orderless, otherwise first component is ignored, unless `corfu-separator'
;;   ;; is inserted.
;;   (corfu-quit-at-boundary nil)
;;   (corfu-preselect-first t)             ; Preselect first candidate?

;;   ;; Other
;;   ;; NOTE 2022-02-05: In my actual configuration, I have this variable set to nil
;;   ;; since I use `corfu-doc', whose configuration comes later. But if you don't
;;   ;; use `corfu-doc', this might be helpful to you.
;;   (corfu-echo-documentation t)      ; Show documentation in echo area?

;;   ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
;;   ;; want to perform completion
;;   (tab-always-indent 'complete)
;;   (completion-cycle-threshold nil) ; Always show all candidates in popup menu

;;   (lsp-completion-provider :none) ; Use corfu instead the default for lsp completions
  
;;   ;; :bind (:map corfu-map
;;   ;;             ("C-n" . corfu-next)
;;   ;;             ("C-p" . corfu-previous)
;;   ;;             ("<escape>" . corfu-quit)
;;   ;;             ("<return>" . corfu-insert)
;;   ;;             ("M-d" . corfu-show-documentation)
;;   ;;             ("M-l" . corfu-show-location))
  
;;   ;; :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
    
;;   :config
;;   ;; Setup lsp to use corfu for lsp completion
;;   (defun kb/corfu-setup-lsp ()
;;     "Use orderless completion style with lsp-capf instead of the
;;   default lsp-passthrough."
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
  
;;   (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;; NOTE DS - unable to use this no-littering function not available, may be
  ;; Emacs 30?
  ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

;;
;; If you want auto completion - i.e. do not have press tab
;; we can configure the settings
;;
;; (setq corfu-auto        t
;;       corfu-auto-delay  0.25 
;;       corfu-auto-prefix 2 
;;       completion-styles '(basic orderless))



(provide 'ds-corfu)

;;; ds-corfu.el ends here
