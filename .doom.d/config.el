(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(after! which-key
  (setq which-key-idle-delay 0.1))

(setq beacon-mode 1)

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-gruvbox)

(set-frame-parameter (selected-frame) 'alpha '(92 100))

(add-to-list 'default-frame-alist '(alpha 95 100))

(setq doom-font (font-spec :family "Iosevka" :size 22 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

(require 'org-ref)

(setq org-directory "~/org/")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-src-fontify-natively t)

(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

(after! latex
  (set-company-backend! 'latex-mode '(company-yasnippet)))

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

(add-hook 'TeX-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-rewrap-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
(define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)
(sp-pair  "\\left(" "\\right)" :insert "M-p")
(sp-pair "(" ")" :wrap "S-C-8")
(sp-pair "[" "]" :insert "S-M-8")



(bind-key "C-M-s"
          (defhydra smartparens-hydra ()
            "smartparens"
            ("d" sp-down-sexp "Down")
            ("D" sp-backward-down-sexp "Backward-Down")
            ("u" sp-backward-up-sexp "Up")
            ("U" sp-up-sexp "Backward-Up")
            ("n" sp-next-sexp "Down")
            ("N" sp-previous-sexp "Down")
            ("c" sp-rewrap-sexp "Change Surround")
            ("<backspace>" sp-backward-unwrap-sexp "Delete Surround" :color red)
            ("h" sp-show-enclosing-pair "show")
            ("f" sp-forward-sexp "Forward")
            ("b" sp-backward-sexp "Backward")
            ("q" nil "Quit" :color blue))
          smartparens-mode-map)



;;;;;;;;;;;;;;;;;;
;; pair management

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

(require 'org-mime)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.mu4e/.mbsyncrc -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  mu4e-view-prefer-html t
  mu4e-update-interval 180
  mu4e-headers-auto-update t
  mu4e-compose-signature-auto-include nil
  mu4e-compose-format-flowed t)

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
  (lambda()
;; try to emulate some of the eww key-bindings
(local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
(local-set-key (kbd "<tab>") 'shr-next-link)
(local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
	(interactive)
	(setq mu4e-headers-fields
	      `((:human-date . 25) ;; alternatively, use :date
		(:flags . 6)
		(:from . 22)
		(:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		(:size . 7)))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
       (flyspell-mode)))

(require 'smtpmail)

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "work" ;;for acc1-gmail
    :enter-func (lambda () (mu4e-message "Entering context work"))
    :leave-func (lambda () (mu4e-message "Leaving context work"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "acc1@googlemail.com")))
    :vars '((user-mail-address . "acc1@googlemail.com")
	    (user-full-name . "Marcus Weber")
	    (mu4e-sent-folder . "/acc1-gmail/[acc1].Gesendet")
	    (mu4e-drafts-folder . "/acc1-gmail/[acc1].Entwürfe")
	    (mu4e-trash-folder . "/acc1-gmail/[acc1].Papierkorb")
	    (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "~/Maildir/acc1-gmail/queue/cur")
	    (message-send-mail-function . smtpmail-send-it)
	    (smtpmail-smtp-user . "acc1")
	    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . ( ("/acc1-gmail/INBOX"            . ?i)
					("/acc1-gmail/[acc1].Gesendet" . ?s)
					("/acc1-gmail/[acc1].Papierkorb"       . ?t)
					("/acc1-gmail/[acc1].Alle Nachrichten"  . ?a)
					("/acc1-gmail/[acc1].Markiert"   . ?r)
					("/acc1-gmail/[acc1].Entwürfe"    . ?d)
					))))
   					))
