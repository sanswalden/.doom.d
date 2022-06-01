;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bill Hensler"
      user-mail-address "sanswalden@gmail.com")

;; (cond (IS-MAC
;;        (setq mac-command-modifier       'meta
;;              mac-option-modifier        'alt
;;              mac-right-option-modifier  'alt
;;              mac-pass-control-to-system nil)))

;; Maximize the starup window
;; from https://rossjhagan.com/thoughts/weekend-with-doom-emacs-clojure/
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq text-scale-mode-step 0.5)
(setq text-scale-mode-step 1.1)

(setq doom-font (font-spec :family "Iosevka SS04" :size 24 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka SS04" :size 20)
      doom-big-font (font-spec :size 28))

;; Randomly pick a Doom logo
(let ((alternatives '("doom-emacs-color2.svg"
                      "doom-emacs-flugo-slant_out_bw-small"
                      "doom-emacs-flugo-slant_out_purple-small.png"
                      "doom-emacs-flugo-slant_out_bw-small.png")))
  (setq fancy-splash-image
        (concat doom-private-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type  'relative)

(setq doom-modeline-enable-word-count t)

;; (set-frame-parameter (selected-frame) 'alpha '(97 95)) ;sets tranparency
;; (add-to-list 'default-frame-alist '(alpha 97 95))

(setq evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")              ; Unicode ellispis are nicer than "...", and also save /precious/ space



;; Shrink the model line height and change the font to match the buffer
;; (setq doom-modeline-height 1)
(custom-set-faces
  '(mode-line ((t (:family "Iosevka SS04" ))))
  '(mode-line-inactive ((t (:family "Iosevka SS04" )))))

(custom-set-faces!
  '(doom-dashboard-banner :inherit default)
  '(doom-dashboard-loaded :inherit default))



;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/emacs-org/org/")
;; same for org-roam
(setq org-roam-directory "~/Documents/emacs-org/org-roam/")


;; allow mixed fonts in an orgbuffer
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
(add-hook! 'org-mode-hook #'solaire-mode)
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))

(setq deft-directory "~/Documents/emacs-org/"
      deft-extensions `("txt" "org")
      deft-recursive t)

(setq org-journal-file-format "%m-%d-%Y.org"
      org-journal-dir "~/Documents/emacs-org/org/journal/")
;;
;; Shared personal dictionary
(setq ispell-personal-dictionary "~/Documents/emacs-org/ispell_english")

(when IS-MAC
  (use-package! org-mac-link
    :after org
    :config
    (setq org-mac-grab-Acrobat-app-p nil) ; Disable grabbing from Adobe Acrobat
    (setq org-mac-grab-devonthink-app-p nil) ; Disable grabbinb from DevonThink
    (map! :map org-mode-map
          "C-c g"  #'org-mac-grab-link)))

(defun my/org-dir-search (dir)
  "Search an org directory using consult-ripgrep. With live-preview."
  (let ((consult-ripgrep-command "rg --null --smart-case --type org --line-buffered --color=always --max-columns=1000 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep dir)))

(map! "<f8>" #'(lambda () (interactive) (my/org-dir-search "~/Documents/emacs-org/")))

(setq-default
 delete-by-moving-to-trash t            ; Delete files to trash
 window-combination-resize t ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(display-time-mode 1)                             ; Enable time in the mode-line

;; (unless (string-match-p "^Power N/A" (battery))   ; On laptops...
;;   (display-battery-mode 1))                       ; it's nice to know how much power you have

;; Window rotation is nice, and can be found under SPC w r and SPC w R. Layout rotation is also nice though. Let’s stash this under SPC w SPC, inspired by Tmux’s use of C-b SPC to rotate windows.
;; We could also do with adding the missing arrow-key variants of the window navigation/swapping commands.
;; For some reason rotate-layout isn't working
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;;LF UTF-8 is the default file encoding, and thus not worth noting in the modeline. So, let’s conditionally hide it.
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


;; I’d like some slightly nicer default buffer names
;;
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")


(setq evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      undo-limit 80000000          ; Raise undo limit to 80 mb
      truncate-string-ellipsis "…" ; Unicode elipeses are nicer
      scroll-margin 2              ; Nice to have some breathing room
      +zen-text-scale 0.3)

;;
;; Avy! What a wonderful way to jump to buffer positions, and it uses the QWERTY home-row for jumping.
;; Very convenient … except I’m using Colemak.
(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

;; For when you want to change the case pattern for a symbol
(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

;; Provide more cider features and zprint for prettier line formatting
(map! (:localleader
       (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
        (:prefix ("e" . "eval")
         ";" #'cider-eval-defun-to-comment)
        (:prefix ("h" . "cheatsheet")
         "s" #'cider-cheatsheet)
        ("=" #'zprint))))

;;  Allow defun evaluation in a (comment ) block
(setq clojure-toplevel-inside-comment-form t)

;; flycheck support to avoid the C-c carpal tunnel
(map!  (:leader :prefix ("C" . "Flycheck")
        :desc "Describe checker"          "?" #'flycheck-describe-checker
        :desc "Clear"                     "C" #'flycheck-clear
        :desc "Show error"                "h" #'flycheck-display-error-at-point
        :desc "Version"                   "V" #'flycheck-version
        :desc "Check buffer"              "c" #'flycheck-buffer
        :desc "Explain error"             "e" #'flycheck-explain-error-at-point
        :desc "Manual"                    "i" #'flycheck-manual
        :desc "List errors"               "l" #'flycheck-list-errors
        :desc "Next error"                "[" #'flycheck-next-error
        :desc "Previous error"            "]" #'flycheck-previous-error
        :desc "Select checker"            "s" #'flycheck-select-checker
        :desc "Verify setup"              "v" #'flycheck-verify-setup
        :desc "Disable checker"           "x" #'flycheck-disable-checker
        :desc "Compile"                   "b" #'flycheck-compile
        :desc "Copy error"                "w" #'flycheck-copy-errors-as-kill))


;; Provide a spacemacs like short cut key ',' for the major mode key. This allows ',' to
;; be a short cut for <space> m to get to the buffers major mode. Mainly wanted to shorten
;; stroke count for eval in clojure
;; (setq doom-localleader-key ",")
;; For some reason, not obvious to me, this didn't work as I expected
;;

;; with lispy [] keys are used as movement in insert mode
;; https://github.com/noctuid/lispyville/issues/36 points out the lispy binds `}' to
;; insert [].
;; (use-package! lispy
;;   (define-key lispy-mode-map-lispy "[" nil)
;;   (define-key lispy-mode-map-lispy "]" nil))

;; color improvements for docs  display
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; In some files, ^L appears as a page break character. This isn’t that visually appealing,
;; and Steve Purcell has been nice enough to make a package to display these as horizontal rules.
(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))

(setq projectile-project-search-path '("~/Documents/emacs-org/" ("~/code" . 1)))

;; org-roam-ui
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
