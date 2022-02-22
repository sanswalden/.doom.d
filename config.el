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
      doom-variable-pitch-font (font-spec :family "Iosevka SS04" :size 20))

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/emacs-org/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type  'relative)

(setq doom-modeline-enable-word-count t)


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

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))

;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :init
;;   (setq org-super-agenda-groups '((name: "Today"
;;                                          :time-grid t
;;                                          :scheduled today)
;;                                   (name: "Due today"
;;                                          :deadline today)
;;                                   (name: "Important"
;;                                          :priority "A")
;;                                   (name: "Overdue"
;;                                          :deadline past)
;;                                   (name: "Due soon"
;;                                          :deadline future)
;;                                   (name: "Big Outcomes"
;;                                          :tag "bo")
;;                                   ))
;;   :config
;; (org-super-agenda-mode))

(setq deft-directory "~/Documents/emacs-org/notes/"
      deft-extensions `("txt" "org")
      deft-recursive t)

(setq org-journal-file-format "%m-%d-%Y.org"
      org-journal-dir "~/Documents/emacs-org/org/journal/")

(when IS-MAC
  (use-package! org-mac-link
    :after org
    :config
    (setq org-mac-grab-Acrobat-app-p nil) ; Disable grabbing from Adobe Acrobat
    (setq org-mac-grab-devonthink-app-p nil) ; Disable grabbinb from DevonThink
    (map! :map org-mode-map
          "C-c g"  #'org-mac-grab-link)))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
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

(map! (:localleader
       (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
        (:prefix ("e" . "eval")
         ";" #'cider-eval-defun-to-comment))))
