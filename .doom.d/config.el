(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")


(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

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

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-dracula)

(set-frame-parameter (selected-frame) 'alpha '(100 100))

(add-to-list 'default-frame-alist '(alpha 100 100))

(setq doom-font (font-spec :family "Iosevka" :size 22 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq lsp-ui-sideline-enable t
      lsp-enable-completion-at-point nil)


(after! rustic
  (setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer))

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

(setq gc-cons-threshold 100000)

(setq treemacs--width-is-locked nil)

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
