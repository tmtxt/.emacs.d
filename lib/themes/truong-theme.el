(deftheme truong
  "Created 2016-02-17.")


(let* (;; colors
       (radio     "#9ACD32")
       (yellow    "#FFFF00")
       (red       "#CD1111")
       (red-l     "#FF0000")
       (red-d     "#8B1A1A")
       (orange    "#FF8C00")
       (golden    "#B8860B")
       (blush     "#F86155")
       (skin      "#D98D54")
       (magenta   "#805DBB")
       (magenta-d "#6A5ACD")
       (blue-l    "#7991E3")
       (blue      "#0084CF")
       (blue-d    "#223360")
       (cyan      "#00CDCD")
       (spring    "#00FA9A")
       (aqua      "#66CDAA")
       (grass     "#00DD00")
       (forest    "#228B22")
       (seaweed   "#2E8B57")

       (bg   "#0C1320")
       (bg+1 "#131A27")
       (bg+2 "#1F2633")
       (bg+3 "#2C3340")
       (fg-3 "#505764")
       (fg-2 "#626976")
       (fg-1 "#747B88")
       (fg   "#858C99")
       (fg+1 "#B8C1C9")

       (fw           `(:inherit fixed-pitch))
       (vw           `(:inherit variable-pitch))
       (vw-italic    `(,@vw :weight light :slant italic))

       (warning      `(:foreground ,red-l))
       (warning-hl   `(:underline (:style wave :color ,orange)))
       (error        `(:foreground ,red-d))
       (error-hl     `(:background ,red-d))
       (power        `(:foreground ,blush))
       (commitment   `(:foreground ,blush))
       (raw          `(:foreground ,skin))
       (minus        `(:foreground ,red))
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
       (spectral-hl  `(:background "#0F1724"))
       (dimmed-hl    `(:background ,bg+1))
       (normal-hl    `(:background ,bg+2))
       (strong-hl    `(:background ,bg+3))
       (special-hl   `(:background ,blue-d))
       (strong       `(:foreground ,fg+1))
       (note         `(:foreground ,magenta))
       (status       `(:background ,blue-l))
       (bold           `(:weight bold)))

  (custom-theme-set-faces
   'truong

   ;; Bases
   `(default
      ((t (,@fw :foreground ,fg+1 :backend ,bg))))
   `(fixed-pitch
     ((t (:family "Hack"))))
   `(variable-pitch
     ((t (:family "Sans Serif"))))
   `(shadow
     ((t (,@dimmed))))
   `(link
     ((t (,@portal :underline ,bg+3))))
   `(button
     ((t (,@fw ,@portal :underline ,bg+3))))

   ;; mode line


   ;; fringe
   `(fringe
     ((t (,@context ,@fw))))
   `(vertical-border
     ((t (,@shadowed))))

   ;; line num
   `(linum
     ((t (:inherit fringe ,@dimmed :slant normal :weight normal
                   :underline nil :strike-through nil :overline nil
                   :background ,bg :box nil :foreground ,radio))))
   `(linum-relative-current-face
     ((t (:inherit (hl-line linum) ,@context ,@bold))))

   ;; highlight
   `(hl-line
     ((t (,@dimmed-hl))))
   `(region
     ((t (,@strong-hl))))
   `(eval-sexp-fu-flash                 ;Flashing eval'ed expression
     ((t (,@normal-hl))))
   `(eval-sexp-fu-flash-error
     ((t (,@warning))))
   `(secondary-selection                ;Highlight changes
     ((t (,@special-hl))))
   `(highlight
     ((t (,@normal-hl))))
   `(match
     ((t (,@strong-hl))))

   ;; search
   `(isearch                            ;current match
     ((t (,@special-hl :foreground ,yellow))))
   `(lazy-highlight                     ;other matches
     ((t (,@special-hl))))
   `(isearch-fail                       ;no match
     ((t (,@error-hl))))

   ;; parens
   `(show-paren-match                   ;matching
     ((t (,@strong))))
   `(show-paren-mismatch                ;unmatched
     ((t (:inherit font-lock-warning-face))))

   ;; flyspell/flycheck
   `(flycheck-error
     ((t (,@error-hl))))
   `(flycheck-warning
     ((t (,@warning-hl))))

   ;; diffs
   `(diff-added
     ((t (,@plus))))
   `(diff-removed
     ((t (,@minus))))
   `(diff-refine-added
     ((t (:inherit diff-added ,@normal-hl))))
   `(diff-refine-removed
     ((t (:inherit diff-removed ,@normal-hl))))
   `(diff-context
     ((t ,context)))
   `(diff-indicator-added
     ((t (:inherit diff-added))))
   `(diff-indicator-removed
     ((t (:inherit diff-removed))))
   `(diff-header
     ((t (:inherit header-line))))
   `(diff-file-header
     ((t (:foreground ,spring))))
   `(diff-hunk-header
     ((t (,@constant :slant italic))))

   `(diff-hl-insert
     ((t (:foreground ,bg :background ,forest))))
   `(diff-hl-delete
     ((t (:foreground ,blush :background ,blush))))
   `(diff-hl-change
     ((t (:foreground ,cyan :background ,cyan))))

   ;; ediff
   `(ediff-even-diff-A
     ((t (,@normal-hl))))
   `(ediff-even-diff-B
     ((t (,@strong-hl))))
   `(ediff-even-diff-C
     ((t (,@strong-hl))))
   `(ediff-even-diff-Ancestor
     ((t (,@strong-hl))))
   `(ediff-odd-diff-A
     ((t (:inherit ediff-even-diff-B))))
   `(ediff-odd-diff-B
     ((t (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-C
     ((t (:inherit ediff-even-diff-Ancestor))))
   `(ediff-odd-diff-Ancestor
     ((t (:inherit ediff-even-diff-C))))

   `(ediff-current-diff-A
     ((t (:background "#210E18"))))
   `(ediff-current-diff-B
     ((t (:background "#092618"))))
   `(ediff-current-diff-C
     ((t (:background "#212618"))))
   `(ediff-current-diff-Ancestor
     ((t (:background "#090F3D"))))
   `(ediff-fine-diff-A
     ((t (:background "#361727"))))
   `(ediff-fine-diff-B
     ((t (:background "#0D3823"))))
   `(ediff-fine-diff-C
     ((t (:background "#374028"))))
   `(ediff-fine-diff-Ancestor
     ((t (:background "#0E165C"))))

   ;; magit
   `(magit-item-highlight
     ((t (,@dimmed-hl))))
   `(magit-section-title
     ((t (,@vw ,@mutable :weight bold))))
   `(magit-branch
     ((t (,@more))))
   `(magit-diff-file-header
     ((t (:inherit diff-file-header))))
   `(magit-diff-hunk-header
     ((t (:inherit diff-hunk-header))))
   `(magit-diff-add
     ((t (:inherit diff-added))))
   `(magit-diff-del
     ((t (:inherit diff-removed))))
   `(magit-diff-none
     ((t (:inherit diff-context))))
   `(magit-log-head-label-default
     ((t (:inherit magit-log-head-label-remote ,@mutable))))
   `(magit-log-sha1
     ((t (,@fw ,@commitment))))
   `(magit-item-mark
     ((t (:inherit secondary-selection))))
   `(magit-tag
     ((t (:foreground ,seaweed :box ,bg+3))))
   `(magit-log-head-label-tags
     ((t (:inherit magit-tag))))
   `(magit-log-author
     ((t (,@vw ,@context))))
   `(magit-log-date
     ((t (,@vw ,@dimmed))))
   `(magit-log-message
     ((t (,@vw-italic))))
   `(magit-key-mode-switch-face
     ((t (:inherit font-lock-type-face))))
   `(magit-blame-header
     ((t (,@vw ,@normal-hl :foreground ,fg-1))))
   `(magit-blame-culprit
     ((t (:inherit magit-blame-header))))
   `(magit-blame-time
     ((t (:inherit magit-blame-header ,@context))))
   `(magit-blame-subject
     ((t (:inherit (magit-log-message magit-blame-header)))))

   `(magit-section-highlight
     ((t (,@normal-hl))))
   `(magit-section-heading
     ((t (,@vw ,@mutable :weight bold))))
   `(magit-branch-local
     ((t (,@more))))
   `(magit-branch-current
     ((t (,@more :box (:color ,fg-2)))))
   `(magit-branch-remote
     ((t (,@doc))))
   `(magit-diff-file-heading
     ((t (:inherit magit-file-name :weight normal))))
   `(magit-diff-file-heading-highlight
     ((t (:inherit magit-diff-file-heading ,@normal-hl :weight bold))))
   `(magit-diff-hunk-heading
     ((t (:inherit diff-hunk-header :overline ,bg+3))))
   `(magit-diff-hunk-heading-highlight
     ((t (:inherit magit-diff-hunk-heading ,@special-hl :overline ,fg :weight bold))))
   `(magit-diff-added
     ((t (:inherit diff-added))))
   `(magit-diff-removed
     ((t (:inherit diff-removed))))
   `(magit-diff-context
     ((t (:inherit diff-context))))
   `(magit-diff-added-highlight
     ((t (:inherit magit-diff-added ,@dimmed-hl))))
   `(magit-diff-removed-highlight
     ((t (:inherit magit-diff-removed ,@dimmed-hl))))
   `(magit-diff-context-highlight
     ((t (:inherit maigt-diff-context ,@dimmed-hl))))
   `(magit-hash
     ((t (,@fw ,@commitment))))
   `(magit-blame-heading
     ((t (,@vw ,@normal-hl :foreground ,fg-1 :overline ,bg+3))))
   `(magit-blame-hash
     ((t (:inherit magit-hash))))
   `(magit-blame-name
     ((t (:inherit magit-blame-heading))))
   `(magit-blame-date
     ((t (:inherit magit-blame-heading ,@context))))
   `(magit-blame-summary
     ((t (:inherit (git-commit-summary magit-blame-heading)))))
   `(magit-popup-argument
     ((t (:inherit font-lock-variable-name-face))))

   ;; font lock
   `(font-lock-builtin-face
     ((t (,@fw ,@constant))))
   `(font-lock-comment-face
     ((t (,@note))))
   `(font-lock-comment-delimiter-face
     ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-string-face
     ((t (,@fw ,@doc))))
   `(font-lock-function-name-face
     ((t (,@fw ,@essence))))
   `(font-lock-keyword-face
     ((t (,@fw ,@power))))
   `(font-lock-reference-face
     ((t (,@fw ,@reference))))
   `(font-lock-regexp-grouping-backslash
     ((t (,@fw ,@spectral))))
   `(font-lock-regexp-grouping-construct
     ((t (,@fw ,@constant))))
   `(font-lock-string-face
     ((t (,@fw ,@string))))
   `(font-lock-doc-face
     ((t (,@string))))
   `(font-lock-type-face
     ((t (,@fw ,@type))))
   `(font-lock-preprocessor-face
     ((t (,@fw ,@raw))))
   `(font-lock-variable-name-face
     ((t (,@fw ,@mutable))))
   `(font-lock-warning-face
     ((t (,@fw ,@warning))))
   `(font-lock-constant-face
     ((t (,@fw ,@constant))))
   `(number-font-lock-face
     ((t (,@fw ,@number))))

   ;; js
   `(js2-function-param
     ((t (,@param))))
   `(js2-jsdoc-type
     ((t (:inherit font-lock-type-face))))
   `(js2-jsdoc-tag
     ((t (:inherit font-lock-builtin-face))))

   `(js3-function-param
     ((t (:inherit js2-function-param))))
   `(js3-jsdoc-type
     ((t (:inherit js2-jsdoc-type))))
   `(js3-jsdoc-tag
     ((t (:inherit js2-jsdoc-tag))))

   ;; web mode
   `(web-mode-preprocessor-face
     ((t (,@context))))
   `(web-mode-html-attr-name-face
     ((t (:inherit font-lock-variable-name-face))))
   `(web-mode-param-name-face
     ((t (:inherit font-lock-constant-face))))
   `(web-mode-html-attr-equal-face
     ((t (,@context))))
   `(web-mode-html-tag-face
     ((t (:inherit font-lock-builtin-face))))
   `(web-mode-html-tag-custom-face
     ((t (:inherit web-mode-html-tag-face :slant italic :underline ,bg+3))))
   `(web-mode-part-face
     ((t (,@dimmed-hl))))
   `(web-mode-block-face
     ((t (,@dimmed-hl))))
   `(web-mode-block-delimiter-face
     ((t (:inherit web-mode-block-face ,@dimmed))))
   `(web-mode-css-selector-face
     ((t (:inherit font-lock-function-name-face))))
   `(web-mode-current-element-highlight-face
     ((t (:inherit highlight))))
   `(web-mode-comment-keyword-face
     ((t (:inherit font-lock-warning-face))))
   `(web-mode-html-tag-bracket-face
     ((t (,@fw ,@context))))
   `(web-mode-folded-face
     ((t (:underline t))))

   ;; nxml
   `(nxml-element-prefix
     ((t (,@fw ,@context))))
   `(nxml-element-local-name
     ((t (:inherit font-lock-builtin-face))))
   `(nxml-tag-delimiter
     ((t (,@fw ,@dimmed))))
   `(nxml-processing-instruction-delimiter
     ((t (:inherit nxml-tag-delimiter))))
   `(nxml-markup-declaration-delimiter
     ((t (:inherit nxml-tag-delimiter))))
   `(nxml-entity-ref-name
     ((t (,@fw :foreground ,skin))))

   ;; org-mode
   `(org-document-title
     ((t (,@string :height 2.0 :bold t))))
   `(org-special-keyword
     ((t (,@fw ,@constant))))
   `(org-indent
     ((t ())))
   `(org-level-1
     ((t (,@vw ,@constant :weight normal :overline ,bg+2 :height 1.4 ,@spectral-hl))))
   `(org-level-2
     ((t (,@vw ,@mutable :weight normal :overline ,bg+2 :height 1.2 ,@spectral-hl))))
   `(org-level-3
     ((t (,@string :weight normal :height 1.1))))
   `(org-level-4
     ((t (:foreground ,cyan :weight normal :height 1.0))))
   `(org-level-5
     ((t (,@param))))
   `(org-level-6
     ((t (:foreground ,radio))))
   ;; `(org-level-7                        ;TODO
   ;;  ((t (:foreground ,green-3))))
   `(org-level-8
     ((t (,@note))))
   `(org-table
     ((t (,@fw :overline ,bg+1 :foreground ,blue-l))))
   `(org-formula
     ((t (,@fw ,@param))))
   `(org-hide
     ((t (:foreground ,bg))))
   `(org-code
     ((t (:inherit font-lock-builtin-face))))
   `(org-verbatim
     ((t (:inherit font-lock-keyword-face))))
   `(org-meta-line
     ((t (,@fw ,@context))))
   `(org-document-info-keyword
     ((t (:inherit org-meta-line))))
   `(org-link
     ((t (:inherit link))))
   `(org-date
     ((t (,@fw :foreground ,cyan :underline t))))
   `(org-todo
     ((t (,@commitment))))
   `(org-block
     ((t (,@fw))))
   `(org-block-background
     ((t (,@fw ,@spectral-hl))))
   `(org-block-begin-line
     ((t (:inherit org-block-background ,@constant :underline ,blue-d))))
   `(org-block-end-line
     ((t (:inherit org-block-background ,@spectral :overline ,blue-d))))
   `(org-checkbox
     ((t (,@fw :weight bold :box (:line-width 1 :style released-button :color ,bg)))))
   `(org-time-grid
     ((t (,@fw ,@context))))
   `(org-agenda-date-today
     ((t (:inherit org-agenda-date :underline t))))
   `(org-agenda-date-weekend
     ((t (:inherit org-agenda-date :slant italic))))
   `(org-agenda-current-time
     ((t (,@fw :inherit org-time-grid ,@header :background ,bg+2))))
   `(org-scheduled-previously
     ((t (,@fw :foreground ,blue-l))))
   `(org-scheduled-today
     ((t (,@fw :foreground ,radio))))
   `(org-agenda-done
     ((t (,@fw :foreground ,seaweed))))
   `(org-tag
     ((t (,@fw ,@context))))
   `(org-column
     ((t (,@fw :slant normal))))
   `(org-priority
     ((t (:foreground ,radio))))
   `(org-headline-done
     ((t (:inherit org-level-2 ,@context))))

   ;; markdown
   `(markdown-link-face
     ((t (,@teleport))))
   `(markdown-url-face
     ((t (:inherit org-link))))
   `(markdown-header-delimiter-face
     ((t (,@dimmed))))
   `(markdown-header-face-1
     ((t (:inherit org-level-1))))
   `(markdown-header-face-2
     ((t (:inherit org-level-2))))
   `(markdown-header-face-3
     ((t (:inherit org-level-3))))
   `(markdown-header-face-4
     ((t (:inherit org-level-4))))
   `(markdown-header-face-5
     ((t (:inherit org-level-5))))
   `(markdown-header-face-6
     ((t (:inherit org-level-6))))
   `(markdown-list-face
     ((t (,@mutable :weight bold))))
   `(markdown-pre-face
     ((t (,@fw ,@dimmed-hl ,@string))))
   `(markdown-bold-face
     ((t (,@mutable :weight bold))))
   `(markdown-italic-face
     ((t (,@mutable :slant italic))))

   ;; helm
   `(helm-header
     ((t (,@vw ,@header))))
   `(helm-source-header
     ((t (,@vw ,@dimmed-hl ,@subheader ,@bold))))
   `(helm-selection
     ((t (:inherit secondary-selection))))
   `(helm-selection-line
     ((t (:inherit secondary-selection))))
   `(helm-match
     ((t (:foreground ,radio ,@bold))))
   `(helm-grep-match
     ((t (:foreground ,yellow))))
   `(helm-ff-file
     ((t (,@portal))))
   `(helm-ff-directory
     ((t (:inherit diredp-dir-priv ,@fw ,dimmed-hl))))
   `(helm-ff-symlink
     ((t (:inherit diredp-symlink))))
   `(helm-ff-executable
     ((t (:inherit diredp-exec-priv))))
   `(helm-separator
     ((t (,@shadowed))))
   `(helm-grep-file
     ((t (,@reference))))
   `(helm-moccur-buffer
     ((t (,@reference))))
   `(helm-grep-lineno
     ((t (,@number))))
   `(helm-action
     ((t (,@vw :height 1.1))))
   `(helm-buffer-directory
     ((t (:inherit helm-ff-directory))))

   ;; dired
   `(diredp-file-name
     ((t (,@vw))))
   `(diredp-dir-priv
     ((t (,@vw ,@more))))
   `(diredp-dir-heading
     ((t (,@header))))
   `(dired-symlink
     ((t (,@teleport))))
   `(diredp-symlink
     ((t (,@teleport))))
   `(diredp-no-priv
     ((t (,@dimmed-hl ,@spectral))))
   `(diredp-read-priv
     ((t (:inherit diredp-no-priv ,@more))))
   `(diredp-write-priv
     ((t (:inherit diredp-no-priv ,@power))))
   `(diredp-exec-priv
     ((t (:inherit diredp-no-priv ,@raw))))
   `(diredp-number
     ((t (,@number))))
   `(diredp-flag-mark-line              ;selected
     ((t (,@special-hl ,@vw))))
   `(diredp-deletion                    ;marked for deletion
     ((t (,@error-hl ,@vw))))
   `(diredp-deletion-file-name
     ((t (,@error-hl))))
   `(diredp-compressed-file-suffix
     ((t (,@constant))))
   `(diredp-file-suffix
     ((t (,@context))))
   ;; `(diredp-ignored-file-name
   ;;   ((t (,@vw-italic ,@note))))

   ;; yasnippet
   `(yas--field-debug-face
     ((t (:underline ,yellow))))
   `(yas-field-highlight-face
     ((t (,@strong-hl))))

   )

  (custom-theme-set-variables
   'truong

   `(hl-paren-colors '("#00FF00"
                       "#00DD00"
                       "#00BB00"
                       "#009900"
                       "#007700"
                       "#005500"))
   ))

(provide-theme 'truong)
