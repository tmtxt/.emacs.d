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

(defcustom tmtxt/alt-fixed-pitch-font "Fantasque Sans Mono"
  "Alternative fixed-pitch font to use for certain faces."
  :group 'tmtxt-dark)

(deftheme tmtxt-dark "Tmtxt Dark theme")

(provide-theme 'tmtxt-dark)
