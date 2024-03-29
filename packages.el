;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
(package! org-super-agenda)
;; The rotate package just adds the ability to rotate window layouts
(package! rotate :pin "4e9ac3ff800880bd9b705794ef0f7c99d72900a6")

;; For when you want to change the case pattern for a symbol.
(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")

(when IS-MAC
  (package! org-mac-link))
(package! zprint-mode)

;; When using org-roam va the '+roam' flag
;; (unpin! org-roam company-org-roam)

(package! olivetti)
(package! exec-path-from-shell)
(package! fountain-mode)
;;
;; color documentation info
(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")

;; Display page break
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))

;; trying out org-roam-ui
(unpin! org-roam)
(package! org-roam-ui)

;; set up a thersaurus to go along with dictionary support via (lookup +dictionary +offline) in init.el
(package! synosaurus)

;; Highlists the current cursor line after major movements. Found via https://ruivieira.dev/doom-emacs.html
(package! beacon)
