;;; org-trello-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-trello" "org-trello.el" (22056 43182 0
;;;;;;  0))
;;; Generated autoloads from org-trello.el

(autoload 'org-trello-version "org-trello" "\
Org-trello version.

\(fn)" t nil)

(autoload 'org-trello-abort-sync "org-trello" "\
Control first, then if ok, add a comment to the current card.

\(fn)" t nil)

(autoload 'org-trello-add-card-comment "org-trello" "\
Control first, then if ok, add a comment to the current card.
When FROM is set, this will delete the current card's comments.

\(fn &optional FROM)" t nil)

(autoload 'org-trello-delete-card-comment "org-trello" "\
Control first, then if ok, delete the comment at point.
This will only work if you are the owner of the comment.

\(fn)" t nil)

(autoload 'org-trello-show-board-labels "org-trello" "\
Control, then if ok, show a simple buffer with the current board's labels.

\(fn)" t nil)

(autoload 'org-trello-sync-card "org-trello" "\
Execute the sync of an entity and its structure to trello.
If FROM is non nil, execute the sync entity and its structure from trello.

\(fn &optional FROM)" t nil)

(autoload 'org-trello-sync-comment "org-trello" "\
Execute the sync of the card's comment at point.
If FROM is non nil, remove the comment at point.

\(fn &optional FROM)" t nil)

(autoload 'org-trello-sync-buffer "org-trello" "\
Execute the sync of the entire buffer to trello.
If FROM is non nil, execute the sync of the entire buffer from trello.

\(fn &optional FROM)" t nil)

(autoload 'org-trello-kill-entity "org-trello" "\
Execute the entity removal from trello and the buffer.
If FROM is non nil, execute all entities removal from trello and buffer.

\(fn &optional FROM)" t nil)

(autoload 'org-trello-kill-cards "org-trello" "\
Execute all entities removal from trello and buffer.

\(fn)" t nil)

(autoload 'org-trello-archive-card "org-trello" "\
Execute archive card at point.

\(fn)" t nil)

(autoload 'org-trello-archive-cards "org-trello" "\
Execute archive all the DONE cards from buffer.

\(fn)" t nil)

(autoload 'org-trello-install-key-and-token "org-trello" "\
No control, trigger setup installation of key and read/write token.

\(fn)" t nil)

(autoload 'org-trello-install-board-metadata "org-trello" "\
Control, if ok, trigger setup installation of trello board to sync with.

\(fn)" t nil)

(autoload 'org-trello-update-board-metadata "org-trello" "\
Control first, then if ok, trigger the update of the informations about the board.

\(fn)" t nil)

(autoload 'org-trello-jump-to-trello-card "org-trello" "\
Jump from current card to trello card in browser.
If FROM is not nil, jump from current card to board.

\(fn &optional FROM)" t nil)

(autoload 'org-trello-jump-to-trello-board "org-trello" "\
Jump to current trello board.

\(fn)" t nil)

(autoload 'org-trello-create-board-and-install-metadata "org-trello" "\
Control first, then if ok, trigger the board creation.

\(fn)" t nil)

(autoload 'org-trello-assign-me "org-trello" "\
Assign oneself to the card.
If UNASSIGN is not nil, unassign oneself from the card.

\(fn &optional UNASSIGN)" t nil)

(autoload 'org-trello-toggle-assign-me "org-trello" "\
Toggling assign/unassign oneself to a card.

\(fn)" t nil)

(autoload 'org-trello-toggle-assign-user "org-trello" "\
Toggling assign one user to a card.

\(fn)" t nil)

(autoload 'org-trello-check-setup "org-trello" "\
Check the current setup.

\(fn)" t nil)

(autoload 'org-trello-delete-setup "org-trello" "\
Delete the current setup.

\(fn)" t nil)

(autoload 'org-trello-help-describing-bindings "org-trello" "\
A simple message to describe the standard bindings used.

\(fn)" t nil)

(autoload 'org-trello-clean-org-trello-data "org-trello" "\
Clean up org-trello data.

\(fn)" t nil)

(autoload 'org-trello-close-board "org-trello" "\
Propose a list of board to and let the user choose which to close.

\(fn)" t nil)

(autoload 'org-trello-mode "org-trello" "\
Sync your org-mode and your trello together.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "org-trello-setup" "org-trello-setup.el" (22056
;;;;;;  43182 0 0))
;;; Generated autoloads from org-trello-setup.el

(defvar org-trello-current-prefix-keybinding nil "\
The default prefix keybinding to execute org-trello commands.")

(custom-autoload 'org-trello-current-prefix-keybinding "org-trello-setup" nil)

;;;***

;;;### (autoloads nil nil ("org-trello-action.el" "org-trello-api.el"
;;;;;;  "org-trello-backend.el" "org-trello-buffer.el" "org-trello-cbx.el"
;;;;;;  "org-trello-controller.el" "org-trello-data.el" "org-trello-date.el"
;;;;;;  "org-trello-deferred.el" "org-trello-entity.el" "org-trello-hash.el"
;;;;;;  "org-trello-input.el" "org-trello-log.el" "org-trello-pkg.el"
;;;;;;  "org-trello-proxy.el" "org-trello-query.el" "org-trello-tools-test.el"
;;;;;;  "org-trello-tools.el" "org-trello-utils.el") (22056 43182
;;;;;;  909208 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-trello-autoloads.el ends here
