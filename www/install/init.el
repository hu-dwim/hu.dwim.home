(unless (and (setq dwim-workspace (or (getenv "DWIM_WORKSPACE")
                                      (let ((dir (expand-file-name "~/workspace")))
                                        (and (file-exists-p dir) dir))
                                      (let ((dir "/opt/hu.dwim.home/workspace"))
                                        (and (file-exists-p dwim-workspace) dir))))
             (file-exists-p dwim-workspace))
  (error "Could not find workspace directory (tried '%s'). Set/change the shell environment variable DWIM_WORKSPACE if you want to use something else." dwim-workspace))

(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.environment/emacs/")))
(require 'dwim-init)

;; (require 'sunrise-commander)
;; (require 'color-moccur)
;; (require 'moccur-edit)
;; (require 'pretty-unicode)

(setq
 slime-default-lisp 'sbcl
 undo-limit 3000000
 undo-strong-limit 5000000)

;; (require 'dwim-key-bindings)
;; (global-set-key (kbd "\\") 'dwim-switch-between-slime-repl-and-last-buffer)
;; (global-set-key (kbd "M-\\") 'dwim-switch-between-debug-and-last-source-buffer)
;; (global-set-key (kbd "C-\\") 'switch-to-other-buffer)
;; (global-set-key (kbd "M-t") 'toggle-truncate-lines)
;; (global-set-key (kbd "M-z") 'redo)
;; (global-set-key (kbd "S-<delete>") 'kill-primary-selection)
;; (global-set-key (kbd "C-<insert>") 'copy-primary-selection)
;; (global-set-key (kbd "S-<insert>") 'yank-clipboard-selection)
;; (global-set-key (kbd "M-c") 'sunrise)

;; (dwim-redefine-ido-key (kbd "C-z") 'undo)
;; (dwim-redefine-ido-key (kbd "M-z") 'redo)
