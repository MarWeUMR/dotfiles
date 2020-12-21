(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

(after! which-key
  (setq which-key-idle-delay 0.1))

;(after! ein:notebook-mode
                                        ;(setq poly-ein-mode t)
                                        ;)

(use-package! polymode)
(use-package! poly-markdown
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown-mode))
  )

(with-eval-after-load "markdown"
  (use-package poly-markdown))

(setq ein:polymode t)
(setq ein:completion-backend 'ein:use-company-backend)

(setq beacon-mode 1)

(after! pdf-view

  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  )

(
 setq doom-variable-pitch-font (font-spec :family "Iosevka")
 doom-big-font (font-spec :family "Iosevka" :size 24)
 doom-serif-font (font-spec :family "Iosevka" :weight 'light)
 )

(after! doom-themes
  (
   setq doom-themes-enable-bold t
   doom-themes-enable-italic t
   )
  )

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
  )

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-dracula)

(set-frame-parameter (selected-frame) 'alpha '(100 100))

(add-to-list 'default-frame-alist '(alpha 100 100))

(setq doom-font (font-spec :family "Iosevka" :size 22 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq gc-cons-threshold 100000)

(setq treemacs--width-is-locked nil
                                        ;doom-themes-treemacs-theme "doom-colors"
      )

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(after! lsp-mode
  (setq
   lsp-enable-completion-at-point nil
   lsp-prefer-capf nil
   lsp-enable-snippet t
   ))


;; (after! lsp-mode
;;   :ensure t
;;   :defer t
;;                                         ;:hook ((lsp-mode . lsp-enable-which-key-integration))
;;   :init ;;
;;   ;; (setq lsp-auto-configure nil)
;;   (setq lsp-prefer-capf nil)
;;   (setq lsp-enable-snippet t)
;;   (setq lsp-keymap-prefix "C-l")
;;   (setq lsp-enable-completion-at-point t)
;;   (setq lsp-keep-workspace-alive nil)
;;   (setq lsp-enable-file-watchers nil)
;;   (setq lsp-enable-semantic-highlighting nil)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-enable-text-document-color nil)
;;   (setq lsp-enable-folding nil)
;;   (setq lsp-enable-indentation nil)
;;   (setq lsp-enable-on-type-formatting nil)
;;   ;; (setq lsp-log-io t)
;;   (add-hook 'lsp-completion-mode-hook (lambda ()
;;                                         (when lsp-completion-mode
;;                                           (set (make-local-variable 'company-backends)
;;                                                (remq 'company-capf company-backends))))))
;; :config (require 'lsp-clients))

(after! rustic
  (setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer)
  )

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("python" "jupyter" "bash" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(require 'org-ref)

(setq org-directory "~/org/")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))



(setq org-src-fontify-natively t)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("myclass"
               "
                \\documentclass[12pt]{scrartcl}
                \\linespread{1.15}
[NO-DEFAULT-PACKAGES]
      [PACKAGES]
\\usepackage[ngerman]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage{csquotes}
\\usepackage{amssymb,amsmath, amsthm, mathtools, mathrsfs}
\\usepackage{my_macros}
\\usepackage[backend= biber]{biblatex}

\\usepackage{scrlayer-scrpage}
\\ohead{Weber, Marcus}
\\ihead{Holzer, Yannik}


\\usepackage{float}
\\usepackage{caption}
\\captionsetup[figure]{labelfont={bf},name={Abbildung},labelsep=period}
\\captionsetup[table]{labelfont={bf},name={Tabelle},labelsep=period}
\\usepackage{hyperref}
\\usepackage{graphicx}
\\usepackage{geometry}
\\geometry{
 a4paper,
 left=25mm,
 right=25mm,
 top=25mm,
 bottom=20m}
\\usepackage{cleveref}
\\addto\\captionsngerman{
  \\crefname{figure}{Abb.}{Abb.}
  \\Crefname{figure}{Abbildung}{Abbildungen}
  \\crefname{table}{Tab.}{Tab.}
  \\crefname{equation}{Gl.}{Gl.}
\\setlength\\parindent{0pt}
\\usepackage{lmodern}
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}
                "
               )))

(require 'company)
(global-company-mode) ; Turn on in all buffers
(setq company-transformers '(company-sort-by-backend-importance)) ;; Should be sorted
(setq company-idle-delay 0) ; default is 0.5
(setq company-minimum-prefix-length 2) ; default is 4
(setq company-selection-wrap-around t) ; When you reach the final, jump to the first
(company-tng-configure-default)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(after! company
  (define-key company-active-map (kbd "C-p")  'company-select-previous)
  (define-key company-active-map (kbd "C-n")  'company-select-next)
  (define-key company-active-map (kbd "<return>")  'company-complete)
  )

                                        ;("C-n" . company-select-next)

;; ;; yasnippet
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; (require 'yasnippet)
(add-to-list 'load-path "~/.doom.d/snippets")
(yas-global-mode 1)


(require 'company-auctex)
(company-auctex-init)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

                                        ;(setq company-backends '((company-yasnippet company-auctex )))

(setq company-box-icons-unknown 'fa_question_circle)

(setq company-box-icons-elisp
      '((fa_tag :face font-lock-function-name-face) ;; Function
        (fa_cog :face font-lock-variable-name-face) ;; Variable
        (fa_cube :face font-lock-constant-face) ;; Feature
        (md_color_lens :face font-lock-doc-face))) ;; Face

(setq company-box-icons-yasnippet 'fa_bookmark)

(setq company-box-icons-lsp
      '((1 . fa_text_height) ;; Text
        (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
        (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
        (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
        (5 . (fa_cog :foreground "#FF9800")) ;; Field
        (6 . (fa_cog :foreground "#FF9800")) ;; Variable
        (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
        (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
        (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
        (10 . (fa_cog :foreground "#FF9800")) ;; Property
        (11 . md_settings_system_daydream) ;; Unit
        (12 . (fa_cog :foreground "#FF9800")) ;; Value
        (13 . (md_storage :face font-lock-type-face)) ;; Enum
        (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
        (15 . md_closed_caption) ;; Snippet
        (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
        (17 . fa_file_text_o) ;; File
        (18 . md_refresh) ;; Reference
        (19 . fa_folder_open) ;; Folder
        (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
        (21 . (fa_square :face font-lock-constant-face)) ;; Constant
        (22 . (fa_cube :face font-lock-type-face)) ;; Struct
        (23 . fa_calendar) ;; Event
        (24 . fa_square_o) ;; Operator
        (25 . fa_arrows)) ;; TypeParameter
      )

(setq tab-width 4
      LaTeX-item-indent 0 latex "latex"
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list
      '(("PDF Tools" TeX-pdf-tools-sync-view)))
(add-hook
 'TeX-after-compilation-finished-hook
 #'TeX-revert-document-buffer)


(setq-default TeX-parse-self t)
(setq-default TeX-auto-save t)
(setq +latex-viewers '(pdf-tools))

(after! latex
  (setq TeX-save-query nil) ;; Save without asking when invoking TeX commands
  )
(after! tex
  (setq-hook! 'TeX-mode-hook +flyspell-immediately nil) ;; no flyspell check after opening the file
  )

(after! smartparens
  ;; Smartparens for whatever reason treats the insertion of dollar signs and quotes as single characters.
  (setq sp--special-self-insert-commands (delete `TeX-insert-dollar sp--special-self-insert-commands))
  (setq sp--special-self-insert-commands (delete `TeX-insert-quote sp--special-self-insert-commands)))

;;(add-hook 'TeX-mode-hook
;;         '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'LaTeX-mode-hook 'add-my-latex-environments)
(defun add-my-latex-environments ()
  (LaTeX-add-environments
   '("DispWithArrows*" LaTeX-env-label)
   '("arrowEquation" LaTeX-env-label)))

(custom-set-variables
 '(font-latex-math-environments (quote
     ("display" "displaymath" "equation" "eqnarray" "gather" "multline"
      "align" "alignat" "xalignat" "dmath" "arrowEquation" "DispWithArrows*")))
 '(TeX-insert-braces nil)
 '(texmathp-tex-commands (quote (("arrowEquation" env-on) ("DispWithArrows*" env-on))))
 ) ;;Stops putting {} on argumentless commands to "save" whitespace

(setq reftex-label-alist
      '(
        ("DispWithArrows*" ?e nil nil t)
        ("arrowEquation" ?e nil nil t)
        ))

(require 'smartparens-config)
(require 'smartparens-latex)

(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;; -----------------------
;; keybinding management
;; -----------------------

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp) ;; swallow first word after ()
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp) ;; swallow first word before ()
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-forward-barf-sexp) ;; push last word out of ()
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-barf-sexp) ;; push first word out of ()

(define-key smartparens-mode-map (kbd "C-M-l") 'sp-beginning-of-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-h") 'sp-beginning-of-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-backward-up-sexp)

(define-key smartparens-mode-map (kbd "C-l") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "C-h") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "M-L") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "M-H") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)

;;;;;;;;;;;;;;;;;;
;; pair management

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))
