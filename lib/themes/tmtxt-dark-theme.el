(unless (>= emacs-major-version 24)
  (error "Tmtxt theme requires Emacs 24 or later"))

(defgroup tmtxt-dark nil "")

(defface tmtxt/variable-pitch
  '((t (:inherit variable-pitch)))
  "Base face to inherit from instead of `variable-pitch'.
`variable-pitch' inherits scaling factor from `default', but faces that inherit
`variable-pitch' do not. Therefore we need another variable-pitch base face, so
that all faces can be scaled uniformly.
Note that `fixed-pitch' doesn't suffer from this problem."
  :group 'tmtxt-dark)

(defvar-local tmtxt/text-scale-fw-remapping nil
  "Scale remapping for `fixed-pitch' and its derived faces.")

(defvar-local tmtxt/text-scale-vw-remapping nil
  "Scale remapping for `tmtxt/variable-pitch' and its derived faces.")

(define-advice text-scale-mode (:after (&rest _) tmtxt/scale-base-faces)
  "Additionally scale other base faces so that all faces are
scaled. This \"base face\" trick is used by `tmtxt-dark-theme.el'."
  (let ((ratio (car (last text-scale-mode-remapping))))
    (when tmtxt/text-scale-fw-remapping
      (face-remap-remove-relative tmtxt/text-scale-fw-remapping))
    (when tmtxt/text-scale-vw-remapping
      (face-remap-remove-relative tmtxt/text-scale-vw-remapping))
    (when ratio
      (setq tmtxt/text-scale-vw-remapping
            (face-remap-add-relative 'tmtxt/variable-pitch
                                     :height ratio))
      (setq tmtxt/text-scale-fw-remapping
            (face-remap-add-relative 'fixed-pitch
                                     :height ratio)))
    (force-window-update (current-buffer))))

(defcustom tmtxt/alt-fixed-pitch-font "Hack"
  "Alternative fixed-pitch font to use for certain faces."
  :group 'tmtxt-dark)

(deftheme tmtxt-dark "Tmtxt Dark theme")

(let* ((class     '((class color) (min-colors 257)))

       (radio     "#9ACD32")
       (yellow    "#FFFF00")
       (orange    "#FF8C00")
       (golden    "#B8860B")
       (blush     "#F86155")
       (skin      "#D98D54")
       (magenta   "#805DBB")
       (magenta-d "#6A5ACD")
       ;; (violet "")
       (blue-l    "#7991E3")
       (blue      "#0084CF")
       (blue-d    "#223360")
       (cyan      "#00CDCD")
       ;; (sky       "LightSkyBlue")
       (spring    "#00e08a")
       (aqua      "#66CDAA")
       (grass     "#00DD00")
       (forest    "#228B22")
       (seaweed   "#2E8B57")


       ;; ;; These should probably be used more

       ;; ;; Preprocessor, write priv
       ;; "#D98D54"
       ;; ;; dired header, ido first match, org time grid
       ;; "#EEDD82"
       ;; ;; helm match, helm mode-line bg, failed isearch, something in magit
       ;; "#9ACD32"
       ;; ;; type (f)
       ;; "#00FA9A"
       ;; ;; highlight (b)
       ;; "#223360"


       ;; ;; Used very little, consider using other colors, or something:

       ;; ;; Diff (b)
       ;; "#CD1111"
       ;; ;; Mode-line texts (f)
       ;; "#8B0000"
       ;; ;; Error highlighting (b)
       ;; "#8B1A1A"
       ;; ;; Regex grouping (f)
       ;; "#FF4500"
       ;; ;; org table, org done, org scheduled today (f)
       ;; "#90EE90"
       ;; ;; org date, prompt, link (f)
       ;; "#00CDCD"
       ;; ;; whitespace highlighting (f)
       ;; "#66CDAA"
       ;; ;; mode-line, link (f) (b)
       ;; "#7991E3"
       ;; ;; References? (f)
       ;; "#6A5ACD"
       ;; ;; org-agenda-structure
       ;; "LightSkyBlue"
       ;; ;; org-scheduled-previously
       ;; "#FF7F24"

       ;; (bg   "#0C1320")
       ;; (bg+1 "#131A27")
       ;; (bg+2 "#1F2633")
       ;; (bg+3 "#2C3340")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; ;; 2%
       ;; (bg   "#0e1727")
       ;; (bg+1 "#161e2d")
       ;; (bg+2 "#222a39")
       ;; (bg+3 "#303746")
       ;; (fg-3 "#545b69")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; ;; 3%
       ;; (bg   "#10192b")
       ;; (bg+1 "#182031")
       ;; (bg+2 "#242d3c")
       ;; (bg+3 "#323a49")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; ;; 4%
       ;; (bg   "#111b2e")
       ;; (bg+1 "#192334")
       ;; (bg+2 "#262f3f")
       ;; (bg+3 "#343c4c")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; ;; 5%
       ;; (bg   "#121e32")
       ;; (bg+1 "#1b2538")
       ;; (bg+2 "#283142")
       ;; (bg+3 "#363f4f")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; ;; 6%
       ;; (bg   "#142036")
       ;; (bg+1 "#1d273b")
       ;; (bg+2 "#2a3446")
       ;; (bg+3 "#384152")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; ;; 7%
       ;; (bg   "#152239")
       ;; (bg+1 "#1e2a3f")
       ;; (bg+2 "#2c3649")
       ;; (bg+3 "#3a4355")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       ;; 8%
       (bg   "#17243d")
       (bg+1 "#202c42")
       (bg+2 "#2e384c")
       (bg+3 "#3c4658")
       (fg-3 "#505764")
       (fg-2 "#626976")
       (fg-1 "#747B88")
       (fg   "#858C99")
       (fg+1 "#B8C1C9")

       ;; (bg   "#0C1320")
       ;; (bg+1 "#131A27")
       ;; (bg+2 "#1F2633")
       ;; (bg+3 "#2C3340")
       ;; (fg-3 "#505764")
       ;; (fg-2 "#626976")
       ;; (fg-1 "#747B88")
       ;; (fg   "#858C99")
       ;; (fg+1 "#B8C1C9")

       (warning      `(:foreground "#FF0000"))
       (error-hl     `(:background "#8B1A1A"))
       (power        `(:foreground ,blush))
       (commitment   `(:foreground ,blush))
       (raw          `(:foreground "#D98D54"))

       (minus        `(:foreground "#CD1111"))
       (plus         `(:foreground ,forest))
       (context      `(:foreground ,fg-3))
       (dimmed       `(:foreground ,bg+3))
       (shadowed     `(:foreground ,bg+2))
       (spectral     `(:foreground ,bg+1))
       (invisible    `(:foreground ,bg))

       (param        `(:foreground ,orange))
       (mutable      `(:foreground ,golden))
       (exception-hl `(:background ,radio))

       (header        `(:foreground "#EEDD82"))
       (subheader     `(:foreground ,golden))

       (essence      `(:foreground ,grass))
       (more         `(:foreground ,grass))

       (string       `(:foreground ,forest))
       (doc          `(:foreground ,seaweed))

       (type         `(:foreground ,spring))
       (portal       `(:foreground ,blue-l))
       (teleport     `(:foreground ,cyan))
       (prompt       `(:foreground ,cyan))

       (constant     `(:foreground ,blue))

       (number       `(:foreground ,seaweed))

       (reference    `(:foreground ,magenta-d))

       ;; (spectral-hl  `(:background "#0F1724"))
       (dimmed-hl    `(:background ,bg+1))
       (normal-hl    `(:background ,bg+2))
       (strong-hl    `(:background ,bg+3))
       (special-hl   `(:background ,blue-d))

       (strong       `(:foreground ,fg+1))

       (note         `(:foreground ,magenta)) ; meta?

       (status       `(:background ,blue-l))

       ;; Mixins

       (italic         `(:slant italic))

       ;; Fixed-width (unscalable)
       (fw0           `(:font ,(face-attribute 'fixed-pitch :font)
                              :weight light
                              :fontset ,(face-attribute 'fixed-pitch :fontset)))
       ;; Variable-width (unscalable)
       (vw0           `(:font ,(face-attribute 'variable-pitch :font)
                              :fontset ,(face-attribute 'variable-pitch :fontset)))
       ;; Fixed-width (scalable)
       (fw             '(:inherit fixed-pitch))
       (fw-alt         `(:inherit fixed-pitch :font ,tmtxt/alt-fixed-pitch-font :height unspecified))
       ;; Variable-width (scalable)
       (vw             '(:inherit tmtxt/variable-pitch))
       (vw-italic      `(,@vw :weight light ,@italic))

       (bold           `(:weight bold)))

  (custom-theme-set-faces
   'tmtxt-dark

   
   ;; Mixin bases. Most faces that wish to always use
   ;; fixed-width/variable-width font should inherit these, not
   ;; `default', which gets font remapped. `text-scale-mode' is
   ;; advised to scale them correctly.
   `(fixed-pitch
     ((,class (,@fw0))))
   `(tmtxt/variable-pitch
     ((,class (,@vw0))))

   `(bold
     ((,class (,@bold :foreground ,fg+1))))

   ;; Bases
   `(default
      ((,class (,@fw0 :foreground ,fg :background ,bg))))
   `(variable-pitch
     ((,class (,@vw0 :foreground ,fg-1))))

   `(shadow ((,class (,@context))))
   `(link ((,class (,@portal :underline ,bg+3))))
   `(button
     ((,class (,@fw ,@portal :underline ,bg+3))))
   `(mouse
     ((,class (:background ,seaweed))))

   ;; mode-line
   `(mode-line
     ((,class (,@fw ,@status :foreground ,bg+1))))
   `(mode-line-inactive
     ((,class (:inherit mode-line ,@normal-hl ,@strong :foreground ,fg-1))))
   `(mode-line-buffer-id
     ((,class (,@vw :weight bold :height 0.9))))
   `(mode-line-highlight
     ((,class (:inherit mode-line))))
   ;; `(which-func                          ;TODO
   ;;   ((,class (:foreground ,red-2 :height 1.0 :bold t))))
   `(anzu-mode-line                     ;TODO
     ((,class (:foreground ,blue-d :weight bold))))

   ;; Fringe
   `(fringe
     ((,class (,@context ,@fw0))))
   `(vertical-border
     ((,class (,@shadowed))))
   ;; Line number
   `(line-number
     ((,class (:inherit fringe ,@dimmed :slant normal :weight normal
                        :underline nil :strike-through nil :overline nil
                        :background ,bg :box nil))))
   `(line-number-current-line
     ((,class (:inherit (hl-line line-number) ,@context ,@bold))))
   `(linum ((,class (:inherit line-number))))
   `(linum-relative-current-face ((,class (:inherit line-number-current-line))))

   
   ;; Highlighting, flyspell, flycheck

   `(hl-line                            ;TODO: Less dimmed
     ((,class (,@dimmed-hl))))
   `(region
     ((,class (,@normal-hl))))
   `(eval-sexp-fu-flash                 ;Flashing eval'ed expression
     ((,class (,@special-hl))))
   `(eval-sexp-fu-flash-error
     ((,class (,@warning))))
   `(secondary-selection                ;Highlight changes
     ((,class (,@special-hl))))
   `(highlight                          ;TODO: What is this?
     ((,class (,@normal-hl))))
   `(match
     ((,class (,@strong-hl))))
   ;; Search
   `(isearch                            ;current match
     ((,class (,@special-hl :foreground ,yellow))))
   `(lazy-highlight                     ;other matches
     ((,class (,@special-hl :foreground ,fg+1))))
   `(isearch-fail                       ;no match
     ((,class (,@error-hl))))
   ;; Parens
   `(show-paren-match                   ;matching
     ((,class (,@strong))))
   `(show-paren-mismatch                ;unmatched
     ((,class (:inherit flyspell-incorrect ,@warning ,@bold))))
   ;; `(tmtxt/lisp-paren-face               ;dimmed
   ;;   ((,class (,@dimmed))))

   ;; flyspell
   `(flyspell-incorrect
     ((,class (:underline (:color ,blush :style wave)))))
   `(flyspell-duplicate
     ((,class (:underline (:color ,radio :style wave)))))
   ;; flycheck
   `(flycheck-error
     ((,class (,@error-hl))))
   `(flycheck-warning
     ((,class (:underline (:color ,bg+3 :style wave)))))

   
   ;; diffs & git

   ;; Inline diffs.
   `(diff-added
     ((,class (,@plus))))
   `(diff-removed
     ((,class (,@minus))))
   `(diff-refine-added
     ((,class (:inherit diff-added ,@strong-hl))))
   `(diff-refine-removed
     ((,class (:inherit diff-removed ,@strong-hl))))
   `(diff-context
     ((,class ,context)))
   `(diff-indicator-added
     ((,class (:inherit diff-added))))
   `(diff-indicator-removed
     ((,class (:inherit diff-removed))))
   `(diff-header
     ((,class (:inherit header-line))))
   `(diff-file-header                   ;TODO
     ((,class (:foreground ,spring))))
   `(diff-hunk-header                   ;TODO
     ((,class (,@constant ,@italic))))

   ;; Side-by-side diff/merge.
   `(ediff-even-diff-A
     ((,class (,@normal-hl))))
   `(ediff-even-diff-B
     ((,class (,@strong-hl))))
   `(ediff-even-diff-C
     ((,class (,@strong-hl))))
   `(ediff-even-diff-Ancestor
     ((,class (,@strong-hl))))
   `(ediff-odd-diff-A
     ((,class (:inherit ediff-even-diff-B))))
   `(ediff-odd-diff-B
     ((,class (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-C
     ((,class (:inherit ediff-even-diff-Ancestor))))
   `(ediff-odd-diff-Ancestor
     ((,class (:inherit ediff-even-diff-C))))
   ;; HSV (V)
   ;; orig: 17 20 20 31
   ;; low: 13 15 15 25
   ;; hi: 21 22 25 36
   `(ediff-current-diff-A
     ((,class (:background "#210E18"))))
   `(ediff-current-diff-B
     ((,class (:background "#092618"))))
   `(ediff-current-diff-C
     ((,class (:background "#212618"))))
   `(ediff-current-diff-Ancestor
     ((,class (:background "#090F3D"))))
   `(ediff-fine-diff-A
     ((,class (:background "#361727"))))
   `(ediff-fine-diff-B
     ((,class (:background "#0D3823"))))
   `(ediff-fine-diff-C
     ((,class (:background "#374028"))))
   `(ediff-fine-diff-Ancestor
     ((,class (:background "#0E165C"))))

   `(magit-tag                          ;TODO
     ((,class (:foreground ,seaweed :box ,fg-2 ,@normal-hl))))
   `(magit-branch-local
     ((,class (,@more ,@special-hl :box ,blue-d))))
   `(magit-branch-current
     ((,class (,@more :box (:color ,fg-2)))))
   `(magit-branch-remote
     ((,class (,@doc ,@special-hl))))

   `(magit-hash
     ((,class (,@fw ,@commitment))))

   `(magit-popup-heading
     ((,class (:inherit font-lock-builtin-face))))
   `(magit-popup-key
     ((,class (:inherit font-lock-keyword-face))))
   `(magit-popup-argument
     ((,class (:inherit font-lock-variable-name-face))))
   `(transient-heading
     ((,class (:inherit font-lock-builtin-face))))
   `(transient-key
     ((,class (:inherit font-lock-keyword-face))))
   `(transient-argument
     ((,class (:inherit font-lock-variable-name-face))))

   ;; Section.
   `(magit-section-heading
     ((,class (,@vw ,@subheader ,@bold))))
   `(magit-section-highlight
     ((,class (,@normal-hl))))
   `(magit-section-secondary-heading
     ((,class (,@vw ,@subheader ,@italic))))
   ;; Files.
   `(magit-diff-file-heading
     ((,class (:inherit magit-filename))))
   `(magit-diff-file-heading-highlight
     ((,class (:inherit magit-diff-file-heading ,@normal-hl ,@bold))))
   ;; Hunks.
   `(magit-diff-hunk-heading
     ((,class (:inherit diff-hunk-header :overline ,bg+3))))
   `(magit-diff-hunk-heading-highlight
     ((,class (:inherit magit-diff-hunk-heading ,@special-hl :overline ,fg ,@bold))))
   ;; Diffs.
   `(magit-diff-added
     ((,class (:inherit diff-added))))
   `(magit-diff-removed
     ((,class (:inherit diff-removed))))
   `(magit-diff-context
     ((,class (:inherit diff-context))))
   `(magit-diff-added-highlight
     ((,class (:inherit magit-diff-added ,@dimmed-hl))))
   `(magit-diff-removed-highlight
     ((,class (:inherit magit-diff-removed ,@dimmed-hl))))
   `(magit-diff-context-highlight
     ((,class (:inherit magit-diff-context ,@dimmed-hl :foreground ,fg-2))))

   ;; Logging.
   `(magit-log-author
     ((,class (,@vw ,@context))))
   `(magit-log-date
     ((,class (,@vw ,@dimmed))))
   `(magit-log-graph
     ((,class (:inherit font-lock-doc-face))))

   ;; Blaming.
   `(magit-blame-heading
     ((,class (,@vw ,@normal-hl :foreground ,fg-1 :overline ,fg-3 :height 0.85))))
   `(magit-blame-hash
     ((,class (:inherit (magit-hash magit-blame-heading) :height 0.85))))
   `(magit-blame-name
     ((,class (:inherit magit-blame-heading))))
   `(magit-blame-date
     ((,class (:inherit magit-blame-heading ,@context))))
   `(magit-blame-summary
     ((,class (:inherit magit-blame-heading ,@subheader))))

   ;; Committing.
   `(git-commit-summary
     ((,class (:inherit magit-log-message))))
   `(git-commit-overlong-summary
     ((,class (:inherit git-commit-summary-face :foreground ,blush))))
   `(git-commit-nonempty-second-line
     ((,class (:inherit git-commit-summary-face ,@error-hl))))
   `(git-commit-comment-heading
     ((,class (:inherit magit-section-heading :weight normal))))
   `(git-commit-comment-action
     ((,class (,@fw ,@commitment))))
   `(git-commit-comment-file
     ((,class (,@vw ,@string))))

   ;; Fringe highlights of diffs.
   `(diff-hl-insert
     ((,class (:foreground ,bg :background ,forest))))
   `(diff-hl-delete
     ((,class (:foreground ,blush :background ,blush))))
   `(diff-hl-change
     ((,class (:foreground ,cyan :background ,cyan))))

   
   ;; Programming languages

   `(font-lock-builtin-face
     ((,class (,@fw ,@constant))))
   `(font-lock-comment-face
     ((,class (,@vw-italic ,@note))))
   `(font-lock-comment-delimiter-face
     ((,class (:inherit font-lock-comment-face ,@dimmed))))
   `(font-lock-doc-face
     ((,class (,@fw-alt ,@italic ,@string))))
   `(font-lock-function-name-face
     ((,class (,@fw ,@essence))))
   `(font-lock-keyword-face
     ((,class (,@fw ,@power))))
   `(font-lock-regexp-grouping-backslash
     ((,class (,@fw ,@dimmed))))
   `(font-lock-regexp-grouping-construct
     ((,class (,@fw ,@constant ,@bold))))
   `(font-lock-string-face
     ((,class (,@fw ,@string))))
   `(font-lock-type-face
     ((,class (,@fw ,@type))))
   `(font-lock-preprocessor-face
     ((,class (,@fw ,@raw))))
   `(font-lock-variable-name-face
     ((,class (,@fw ,@mutable))))
   `(font-lock-warning-face
     ((,class (,@fw ,@warning))))
   `(font-lock-constant-face
     ((,class (,@fw ,@constant))))
   `(highlight-numbers-number
     ((,class (,@fw ,@number))))
   `(lisp-extra-font-lock-quoted
     ((,class (,@fw))))
   `(lisp-extra-font-lock-quoted-function
     ((,class (,@fw :inherit link))))
   `(lisp-extra-font-lock-backquote
     ((,class (:inherit font-lock-preprocessor-face))))
   `(hl-todo
     ((,class (:inherit font-lock-warning-face))))

   `(js2-function-param
     ((,class (,@param))))
   `(js2-jsdoc-type
     ((,class (:inherit font-lock-type-face))))
   `(js2-jsdoc-tag
     ((,class (:inherit font-lock-builtin-face))))

   `(js3-function-param
     ((,class (,@param))))
   `(js3-jsdoc-type
     ((,class (:inherit font-lock-type-face))))
   `(js3-jsdoc-tag
     ((,class (:inherit font-lock-builtin-face))))

   `(web-mode-preprocessor-face
     ((,class (,@context))))
   `(web-mode-html-attr-name-face
     ((,class (:inherit font-lock-variable-name-face))))
   `(web-mode-param-name-face
     ((,class (:inherit font-lock-constant-face))))
   `(web-mode-html-attr-equal-face
     ((,class (,@context))))
   `(web-mode-html-tag-face
     ((,class (:inherit font-lock-builtin-face))))
   `(web-mode-html-tag-custom-face
     ((,class (:inherit web-mode-html-tag-face ,@italic :underline ,bg+3))))
   `(web-mode-part-face
     ((,class (,@dimmed-hl))))
   `(web-mode-block-face
     ((,class (,@dimmed-hl))))
   `(web-mode-block-delimiter-face
     ((,class (:inherit web-mode-block-face ,@dimmed))))
   `(web-mode-css-selector-face
     ((,class (:inherit font-lock-function-name-face))))
   `(web-mode-current-element-highlight-face
     ((,class (:inherit highlight))))
   `(web-mode-comment-keyword-face
     ((,class (:inherit font-lock-warning-face))))
   `(web-mode-html-tag-bracket-face
     ((,class (,@fw ,@context))))
   `(web-mode-folded-face
     ((,class (:underline t))))

   `(nxml-element-prefix
     ((,class (,@fw ,@context))))
   `(nxml-element-local-name
     ((,class (:inherit font-lock-builtin-face))))
   `(nxml-tag-delimiter
     ((,class (,@fw ,@dimmed))))
   `(nxml-processing-instruction-delimiter
     ((,class (:inherit nxml-tag-delimiter))))
   `(nxml-markup-declaration-delimiter
     ((,class (:inherit nxml-tag-delimiter))))
   `(nxml-entity-ref-name
     ((,class (,@fw :foreground ,skin))))

   `(sh-heredoc
     ((,class (,@doc ,@bold))))

   `(rust-question-mark-face
     ((,class (:inherit font-lock-keyword-face ,@bold ,@italic))))
   `(rust-string-interpolation-face
     ((,class (:inherit font-lock-variable-name-face ,@italic))))
   `(rust-unsafe-face
     ((,class (:inherit font-lock-warning-face ,@bold ,@italic
                        :underline (:color ,fg-2 :style wave)))))

   
   ;; Non-HTML markup languages: org-mode, markdown...

   ;; org-mode
   ;; TODO: levels
   `(org-document-title
     ((,class (,@string :height 2.0 :bold t))))
   `(org-special-keyword
     ((,class (,@fw ,@context))))
   `(org-indent                                ;TODO
     ((,class (:inherit (org-hide fixed-pitch) ;; :foreground ,bg+2 :background ,bg+2
                        ))))
   ;; We use `normal' not `bold' for these because we use Fira Sans
   `(org-level-1
     ((,class (,@vw ,@constant :weight normal :height 1.4 ,@dimmed-hl
                    :box (:line-width 1 :color ,bg :style released-button)))))
   `(org-level-2
     ((,class (,@vw ,@subheader :weight normal :height 1.2))))
   `(org-level-3
     ((,class (,@string :weight normal :height 1.1))))
   `(org-level-4
     ((,class (:foreground ,cyan :weight normal :height 1.0))))
   `(org-level-5
     ((,class (,@param))))
   `(org-level-6
     ((,class (:foreground ,radio))))
   ;; `(org-level-7                        ;TODO
   ;;  ((,class (:foreground ,green-3))))
   `(org-level-8
     ((,class (,@note))))
   `(org-table                          ;TODO
     ((,class (,@fw :overline ,bg+1 :foreground ,blue-l))))
   `(org-formula
     ((,class (,@fw ,@param))))
   `(org-hide
     ((,class (:foreground ,bg))))
   `(org-code
     ((,class (:inherit font-lock-builtin-face))))
   `(org-verbatim
     ((,class (:inherit font-lock-builtin-face))))
   `(org-meta-line
     ((,class (,@fw ,@context))))
   `(org-document-info-keyword
     ((,class (:inherit org-meta-line))))
   ;; `(org-mode-line-clock                ;TODO
   ;;  ((,class (:foreground ,blush-2 :bold t))))
   `(org-link
     ((,class (:inherit link))))
   `(org-date                           ;TODO
     ((,class (,@fw :foreground ,cyan :underline t))))
   `(org-todo
     ((,class (,@commitment))))
   ;; `(org-done                           ;TODO
   ;;  ((,class (:foreground ,green-3))))
   `(org-block
     ((,class (,@fw :foreground ,fg))))
   `(org-block-begin-line
     ((,class (:inherit org-block ,@dimmed :underline ,blue-d))))
   `(org-block-end-line
     ((,class (:inherit org-block ,@dimmed :overline ,blue-d))))
   `(org-checkbox
     ((,class (,@fw ,@bold ,@commitment :height 1.1))))
   `(org-checkbox-done-text
     ((,class (:foreground ,fg-3 :strike-through t))))
   `(org-time-grid
     ((,class (,@fw ,@context))))
   ;; `(org-agenda-structure               ;TODO
   ;;  ((,class (,@fw :foreground "LightSkyBlue"))))
   `(org-agenda-date-today
     ((,class (:inherit org-agenda-date :underline t))))
   `(org-agenda-date-weekend
     ((,class (:inherit org-agenda-date ,@italic))))
   `(org-agenda-current-time
     ((,class (,@fw :inherit org-time-grid ,@header :background ,bg+2))))
   `(org-scheduled-previously
     ((,class (,@fw :foreground ,blue-l))))
   `(org-scheduled-today
     ((,class (,@fw :foreground ,radio))))
   `(org-agenda-done
     ((,class (,@fw :foreground ,seaweed))))
   `(org-tag
     ((,class (,@fw ,@context))))
   ;; `(org-scheduled                      ;TODO
   ;;  ((,class (,@vw-italic :foreground ,green-2))))
   ;; `(org-scheduled-previously           ;TODO
   ;;  ((,class (,@fw :foreground "Chocolate1" ,@italic))))
   ;; `(org-scheduled-today                ;TODO
   ;;  ((,class (,@vw :foreground ,green-3))))
   `(org-column
     ((,class (,@fw :slant normal))))
   `(org-priority
     ((,class (:foreground ,radio))))
   `(org-headline-done
     ((,class (:inherit org-level-2 ,@context))))

   `(markdown-link-face
     ((,class (,@teleport))))
   `(markdown-url-face
     ((,class (:inherit org-link))))
   `(markdown-header-delimiter-face
     ((,class (,@dimmed))))
   `(markdown-header-face-1
     ((,class (:inherit org-level-1))))
   `(markdown-header-face-2
     ((,class (:inherit org-level-2))))
   `(markdown-header-face-3
     ((,class (:inherit org-level-3))))
   `(markdown-header-face-4
     ((,class (:inherit org-level-4))))
   `(markdown-header-face-5
     ((,class (:inherit org-level-5))))
   `(markdown-header-face-6
     ((,class (:inherit org-level-6))))
   `(markdown-list-face
     ((,class (,@subheader :weight bold))))
   `(markdown-pre-face
     ((,class (,@fw ,@dimmed-hl ,@string))))
   `(markdown-bold-face
     ((,class (,@mutable :weight bold))))
   `(markdown-italic-face
     ((,class (,@mutable ,@italic))))
   `(markdown-inline-code-face
     ((,class (:inherit org-code))))
   `(markdown-code-face
     ((,class (:inherit org-block))))
   `(markdown-language-keyword-face
     ((,class (:inherit org-block-begin-line))))
   `(markdown-markup-face
     ((,class (,@dimmed))))
   `(markdown-html-tag-delimiter-face
     ((,class (,@fw ,@dimmed))))

   `(rst-level-1
     ((,class (:inherit org-level-1))))
   `(rst-level-2
     ((,class (:inherit org-level-2))))
   `(rst-level-3
     ((,class (:inherit org-level-3))))
   `(rst-adornment
     ((,class (,@context))))
   `(rst-block
     ((,class (:inherit markdown-list-face))))
   `(rst-literal
     ((,class (:inherit org-code))))
   `(rst-reference
     ((,class (:inherit org-code))))
   `(rst-definition
     ((,class (:inherit org-link))))
   `(rst-emphasis1
     ((,class (:inherit markdown-italic-face))))
   `(rst-emphasis2
     ((,class (:inherit markdown-bold-face))))

   `(markup-title-0-face
     ((,class (:inherit org-document-title))))
   `(markup-title-1-face
     ((,class (:inherit org-level-1))))
   `(markup-title-2-face
     ((,class (:inherit org-level-2))))
   `(markup-title-3-face
     ((,class (:inherit org-level-3))))
   `(markup-title-4-face
     ((,class (:inherit org-level-4))))
   `(markup-title-5-face
     ((,class (:inherit org-level-5))))
   `(markup-internal-reference-face
     ((,class (:inherit org-link))))
   `(markup-typewriter-face
     ((,class (:inherit org-code))))
   `(markup-code-face
     ((,class (:inherit markdown-pre-face))))
   `(markup-list-face
     ((,class (:inherit markdown-list-face))))

   
   ;; helm

   ;; Base
   `(helm-header
     ((,class (,@vw ,@header))))        ; TODO
   `(helm-source-header
     ((,class (,@vw ,@dimmed-hl ,@subheader ,@bold)))) ; TODO
   `(helm-separator
     ((,class (,@shadowed))))
   `(helm-match
     ((,class (:foreground ,radio ,@bold))))
   `(helm-selection
     ((,class (:inherit secondary-selection))))
   `(helm-selection-line
     ((,class (:inherit secondary-selection))))
   `(helm-action
     ((,class (,@vw :height 1.1))))

   `(helm-grep-match
     ((,class (:foreground ,yellow))))
   `(helm-ff-file
     ((,class (,@portal))))
   `(helm-ff-directory
     ((,class (:inherit diredfl-dir-priv ,@fw ,dimmed-hl))))
   `(helm-ff-symlink
     ((,class (:inherit diredfl-symlink))))
   `(helm-ff-executable
     ((,class (:inherit diredfl-exec-priv))))
   ;; `(helm-candidate-number
   ;;   ((,class (:background ,yellow-1 :foreground ,bg :bold t)))) ; TODO
   `(helm-grep-file
     ((,class (,@reference))))
   `(helm-moccur-buffer                 ;TODO
     ((,class (,@reference))))
   `(helm-non-file-buffer
     ((,class (:foreground ,fg-2 ,@italic))))
   `(helm-grep-lineno
     ((,class (,@number))))
   ;; `(helm-grep-finish                   ;TODO
   ;;   ((,class (,@doc))))

   `(helm-buffer-directory
     ((,class (:inherit helm-ff-directory))))
   `(helm-buffer-process
     ((,class (,@doc ,@vw-italic :height 0.85))))
   `(helm-buffer-size
     ((,class (,@dimmed))))
   `(helm-buffer-modified
     ((,class (,@commitment))))
   `(helm-swoop-target-line-face
     ((,class (,@special-hl))))
   `(helm-swoop-target-line-block-face
     ((,class (,@special-hl))))
   `(helm-swoop-target-word-face
     ((,class (:inherit isearch))))
   `(helm-swoop-line-number-face
     ((,class (,@number))))

   
   ;; dired

   `(diredfl-dir-heading
     ((,class (,@header ,@vw))))
   `(diredfl-number
     ((,class (,@number))))

   ;; Type
   `(diredfl-file-name                  ;TODO
     ((,class (,@vw))))
   `(diredfl-dir-name
     ((,class (,@teleport))))
   `(dired-symlink
     ((,class (,@portal ,@italic :underline ,bg+3))))
   `(diredfl-symlink
     ((,class (:inherit dired-symlink))))
   `(diredfl-ignored-file-name
     ((,class (,@italic ,@note))))

   `(diredfl-file-suffix
     ((,class (,@context ,@italic))))
   `(diredfl-compressed-file-suffix     ;TODO
     ((,class (,@constant))))

   ;; Permission bits
   `(diredfl-no-priv
     ((,class (,@dimmed-hl ,@spectral))))
   `(diredfl-read-priv
     ((,class (:inherit diredfl-no-priv ,@more))))
   `(diredfl-write-priv
     ((,class (:inherit diredfl-no-priv ,@power))))
   `(diredfl-exec-priv
     ((,class (:inherit diredfl-no-priv ,@raw))))
   `(diredfl-rare-priv
     ((,class (,@special-hl ,@power ,@italic))))
   `(diredfl-dir-priv
     ((,class (,@special-hl ,@power ,@italic))))

   ;; Selection
   `(diredfl-flag-mark-line             ;selected
     ((,class (,@special-hl))))
   `(diredfl-flag-mark
     ((,class (:inherit diredfl-flag-mark-line))))
   `(diredfl-deletion-file-name         ;marked for deletion
     ((,class (,@error-hl))))
   `(diredfl-deletion
     ((,class (:inherit diredfl-deletion-file-name))))

   `(dired-rainbow-executable-face
     ((,class (:inherit diredfl-exec-priv))))

   
   ;; REPLs

   `(comint-highlight-prompt
     ((,class (,@prompt))))
   `(comint-highlight-input
     ((,class (:background ,bg+1))))

   `(cider-repl-prompt-face
     ((,class (:inherit font-lock-constant-face))))
   `(cider-repl-input-face
     ((,class (:inherit comint-highlight-input))))
   `(cider-repl-result-face
     ((,class (,@constant))))           ; TODO

   `(eshell-prompt
     ((,class (,@prompt))))

   
   ;; Help

   ;; info
   `(info-title-1
     ((,class (:inherit org-level-1 ,@bold))))
   `(info-title-2
     ((,class (:inherit org-level-2 ,@bold))))
   `(info-title-3
     ((,class (:inherit org-level-3 ,@bold))))
   `(info-title-4
     ((,class (:inherit org-level-4))))
   `(info-menu-header
     ((,class (:inherit org-level-2 ,@bold))))
   `(info-xref
     ((,class (,@portal))))
   `(info-xref-visited
     ((,class (:inherit info-xref ,@note ,@italic))))
   `(info-header-node
     ((,class (:inherit info-title-4 ,@bold ,@italic))))
   `(Info-quoted
     ((,class (,@fw ,@constant))))
   `(info-menu-star
     ((,class ())))

   ;; info-colors
   `(info-colors-ref-item-type
     ((,class (,@context ,@bold ,@italic))))
   `(info-colors-ref-item-user-option
     ((,class (:inherit font-lock-variable-name-face))))
   `(info-colors-ref-item-variable
     ((,class (:inherit info-colors-ref-item-user-option ,@italic))))
   `(info-colors-lisp-code-block
     ((,class (:inherit org-block ,@dimmed-hl :weight normal))))
   `(info-colors-ref-item-special-form
     ((,class (:inherit info-colors-ref-item-macro :underline t))))

   ;; apropos
   `(apropos-symbol
     ((,class (:inherit info-reference-item))))
   `(apropos-function-button
     ((,class (:inherit info-function-ref-item))))
   `(apropos-variable-button
     ((,class (:inherit info-variable-ref-item))))
   `(apropos-misc-button
     ((,class (:inherit info-constant-ref-item))))

   `(help-argument-name                 ;TODO
     ((,class (,@fw :foreground ,blue))))
   `(describe-variable-value
     ((,class (,@fw ,@doc))))

   `(helpful-heading
     ((,class (:inherit org-level-3))))

   ;; man/woman
   `(woman-bold                         ;TODO
     ((,class (,@constant))))
   `(woman-italic
     ((,class (:foreground ,spring))))
   `(woman-addition                     ;TODO
     ((,class (,@mutable))))
   `(woman-unknown                      ;TODO
     ((,class (:foreground ,blush))))
   `(Man-overstrike
     ((,class (:inherit woman-bold))))
   `(Man-underline
     ((,class (:inherit woman-italic))))

   
   ;; IRC

   `(erc-notice-face
     ((,class (,@dimmed))))
   `(erc-nick-default-face              ;TODO
     ((,class (,@string))))
   `(erc-current-nick-face
     ((,class (,@constant))))
   `(erc-my-nick-face
     ((,class (,@constant))))
   `(erc-timestamp-face
     ((,class (,@note))))
   `(erc-prompt-face
     ((,class (,@prompt :bold t))))
   `(erc-command-indicator-face
     ((,class (,@italic :weight normal))))
   `(erc-button
     ((,class (:slant normal))))

   
   ;; Popups

   `(company-tooltip
     ((,class (,@fw ,@strong-hl :foreground ,aqua))))
   `(company-tooltip-selection
     ((,class (:inherit company-tooltip :foreground ,cyan :background ,fg-3))))
   `(company-tooltip-common
     ((,class (:inherit company-tooltip ,@subheader))))
   `(company-tooltip-common-selection
     ((,class (:inherit company-tooltip-selection ,@param))))
   `(company-tooltip-annotation
     ((,class (:inherit company-tooltip :foreground ,seaweed))))
   `(company-scrollbar-bg
     ((,class (:background ,fg-3))))
   `(company-scrollbar-fg
     ((,class (:background ,seaweed))))
   `(company-preview
     ((,class (:foreground ,fg-3))))
   `(company-preview-common
     ((,class (:inherit company-preview))))

   
   ;; Hydra
   `(hydra-face-red
     ((,class (:inherit font-lock-keyword-face))))
   `(hydra-face-blue
     ((,class (:inherit font-lock-constant-face))))
   `(hydra-face-amaranth
     ((,class (:inherit hydra-face-red ,@bold))))
   `(hydra-face-pink
     ((,class (,@note ,@bold))))
   `(hydra-face-teal
     ((,class (,@teleport))))

   
   ;; Misc

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,class (,@dimmed))))
   `(undo-tree-visualizer-current-face
     ((,class (,@essence))))
   `(undo-tree-visualizer-active-branch-face ;TODO
     ((,class (,@string))))

   `(compilation-info
     ((,class (,@reference))))
   `(compilation-error
     ((,class (,@warning))))
   `(compilation-line-number            ;TODO
     ((,class (,@number))))

   ;; XXX: Why aren't these even registered?
   `(hexl-address-region
     ((,class (:foreground ,blue))))
   `(hexl-ascii-region
     ((,class (:foreground ,blue))))

   ;; yasnippet
   `(yas--field-debug-face
     ((,class (:underline ,yellow))))
   `(yas-field-highlight-face
     ((,class (,@strong-hl))))

   `(avy-background-face
     ((,class (:foreground ,bg+3))))
   `(avy-lead-face
     ((,class (:underline nil :box nil
                          :strike-through nil :inverse-video nil :overline nil
                          :background ,bg :foreground ,yellow
                          :inherit t))))
   `(avy-lead-face-0
     ((,class (:inherit avy-lead-face))))

   ;; Twitter
   `(twittering-uri-face
     ((,class (:inherit link))))
   `(twittering-username-face           ;TODO
     ((,class (,@string))))

   ;; My own custom faces
   ;; `(ublt-twitter-meta-face
   ;;   ((,class (:height 0.8 ,@shadowed))))
   ;; `(ublt-twitter-meta-location
   ;;   ((,class (:height 0.8 ,@spectral))))
   ;; `(ublt-twitter-meta-reply
   ;;   ((,class (:height 0.8 :foreground ,blue-d))))
   ;; `(ublt-twitter-meta-retweet
   ;;   ((,class (:height 0.8 :foreground ,blue))))
   ;; `(ublt/flycheck-message-face         ;TODO
   ;;   ((,class (,@vw ,@commitment))))
   ;; `(ublt/mode-line-major-mode
   ;;   ((,class (:bold t))))
   ;; `(ublt/evil-emacs-tag
   ;;   ((,class (,@fw ,@error-hl :foreground ,radio :weight bold))))
   ;; `(ublt/evil-motion-tag
   ;;   ((,class (,@fw ,@exception-hl :foreground ,blue-d :weight bold))))
   ;; `(ublt/evil-normal-tag
   ;;   ((,class (,@fw :foreground ,blue-d :weight bold))))
   ;; `(ublt/evil-insert-tag
   ;;   ((,class (,@fw ,@error-hl :foreground ,blue-l :weight bold))))
   ;; `(ublt/evil-visual-tag
   ;;   ((,class (,@fw ,@special-hl :foreground ,blush :weight bold))))

   )
  (custom-theme-set-variables
   'tmtxt-dark

   `(hl-paren-colors '("#00FF00"
                       "#00DD00"
                       "#00BB00"
                       "#009900"
                       "#007700"
                       "#005500"))
   )
  )

(provide-theme 'tmtxt-dark)
