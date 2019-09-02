;;; Config specific for working with AR project

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for parsing version and string

(defun tmtxt/extract-version (str)
  "Extract version from a string. Ex: integrations-v1.1.0 -> 1.1.0"
  (->> str
       (s-match "[0-9]+\.[0-9]+\.[0-9]+")
       (-last-item)))

(defun tmtxt/compare-version-strings (str1 str2)
  "Extract the version string the 2 input strings and compare them. Return true if tag1 < tag2, false if tag1 > tag2
   Ex: microservice.realm-v0.0.3 and microservice.realm-v0.1.2
   Return true in the example above
   "
  (let* ((version1 (tmtxt/extract-version str1))
         (version2 (tmtxt/extract-version str2)))
    (version< version1 version2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for comparing and computing the next version

;;; Helper functions to compute the next major/minor/patch version
(defun tmtxt/get-next-major-version (current-version)
  "Get the next major version of the current-version string.
   Ex: current-version = 1.2.3, next-major-version = 2.0.0"
  (->> current-version
       ;; convert the version string to list of int
       (version-to-list)
       ;; increase the major version
       (-update-at 0 '1+)
       ;; reset minor and patch to 0
       (-replace-at 1 0)
       (-replace-at 2 0)
       ;; join back to a string
       (-map 'number-to-string)
       (s-join ".")))

(defun tmtxt/get-next-minor-version (current-version)
  "Get the next minor version of the current-version string.
   Ex: current-version = 1.2.3, next-major-version = 1.3.0"
  (->> current-version
       ;; convert the version string to list of int
       (version-to-list)
       ;; increase the minor version
       (-update-at 1 '1+)
       ;; reset patch to 0
       (-replace-at 2 0)
       ;; join back to a string
       (-map 'number-to-string)
       (s-join ".")))

(defun tmtxt/get-next-patch-version (current-version)
  "Get the next patch version of the current-version string.
   Ex: current-version = 1.2.3, next-major-version = 1.2.4"
  (->> current-version
       ;; convert the version string to list of int
       (version-to-list)
       ;; increase the patch version
       (-update-at 2 '1+)
       ;; join back to a string
       (-map 'number-to-string)
       (s-join ".")))

(defun tmtxt/get-next-version (current-version version-type)
  "Get the next version
   version-type: major/minor/patch
   Ex: current-version: 4.5.6 -> next major 5.0.0"
  (cond
   ((string= version-type "major") (tmtxt/get-next-major-version current-version))
   ((string= version-type "minor") (tmtxt/get-next-minor-version current-version))
   (t (tmtxt/get-next-patch-version current-version))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AR specific features

;;; Whether to enable the feature or not
(let* (;; list of possible locations of AR Connect repo
       (connect-dirs '("~/Projects/connect" "c:/Users/me/Projects/connect"))

       ;; find the one that exists
       (connect-dir (-find 'file-directory-p connect-dirs)))
  (if connect-dir
      (progn
        ;; Enable the feature
        (defconst AR-FEATURES-ENABLED t)
        ;; Path to AR Connect repo
        (defconst AR-CONNECT-PATH connect-dir))
    (defconst AR-FEATURES-ENABLED nil)))

;;; Only define the list of services when the feature is enabled
(when AR-FEATURES-ENABLED
  ;; Define list of AR services when the feature is enabled
  (progn
    ;; list of frontend services
    (defconst AR-FRONTEND-SERVICES (-> AR-CONNECT-PATH
                                       (s-concat "/frontend")
                                       (directory-files)))

    ;; list of nodejs services
    (defconst AR-NODE-SERVICES (-> AR-CONNECT-PATH
                                   (s-concat "/nodejs")
                                   (directory-files)))

    ;; list of golang services
    ;; golang services need .golang suffix
    (defconst AR-GOLANG-SERVICES (--> AR-CONNECT-PATH
                                      (s-concat it "/golang")
                                      (directory-files it)
                                      (-map (lambda (service) (s-concat service ".golang")) it)))

    ;; list of c# services
    ;; c# services need .csharp suffix
    (defconst AR-CSHARP-SERVICES (--> AR-CONNECT-PATH
                                      (s-concat it "/csharp")
                                      (directory-files it)
                                      (-map (lambda (service) (s-concat service ".csharp")) it)))

    ;; list of integration test services
    (defconst AR-INTEGRATION-TEST-SERVICES (--> AR-CONNECT-PATH
                                                (s-concat it "/tests")
                                                (directory-files it)))

    ;; all services
    (defconst AR-ALL-SERVICES (-distinct (-concat AR-FRONTEND-SERVICES
                                                  AR-NODE-SERVICES
                                                  AR-GOLANG-SERVICES
                                                  AR-CSHARP-SERVICES
                                                  AR-INTEGRATION-TEST-SERVICES)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for user input
(defun ar/select-service ()
  "Ask for user input for 1 service name from one the above"
  (helm-comp-read "Select service to tag: " AR-ALL-SERVICES))

(defun ar/select-version-type ()
  "Ask for user input 1 version type"
  (helm-comp-read "Select next version type: " (list "patch" "minor" "major")))

(defun ar/select-tag-type ()
  "Ask for user input 1 tag type"
  (helm-comp-read "Select next tag type: " (list "unit-test" "cascading-unit-test" "hot-fix")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for computing the next git tag
(defun ar/get-latest-tag (service-name)
  "Get the latest tag for one service name"
  (let ((default-directory AR-CONNECT-PATH)
        (regex (s-concat service-name "-v")))
    (--> "tag"
         ;; get all git tags for this repo
         (magit-git-lines it)
         ;; filter to the tags that match the service name
         (-filter (lambda (tag) (s-contains? regex tag)) it)
         ;; sort the tags, the oldest first
         (-sort 'tmtxt/compare-version-strings it)
         ;; pick the last tag (the latest one)
         (-last-item it))))

(defun ar/get-tag-suffix (tag-type)
  "Get tag suffix"
  (cond
   ((string= tag-type "unit-test") "-unit-test")
   ((string= tag-type "cascading-unit-test") "-cascading-unit-test")
   (t "")))

(defun ar/get-next-tag (service-name tag-name version-type tag-type)
  "Get next tag for the input tag-name
   service-name: microservice.realm
   tag-name: microservice.realm-v0.0.3
   version-type: major
   tag-type: unit-test
   => microservice.realm-v1.0.0-unit-test"
  (let* ((tag-name-without-version (s-concat service-name "-v"))
         (next-version (-> tag-name
                           (tmtxt/extract-version)
                           (tmtxt/get-next-version version-type)))
         (tag-suffix (ar/get-tag-suffix tag-type)))
    (s-concat tag-name-without-version next-version tag-suffix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for increasing the tag
(defun ar/increase-version (version-type)
  "Ask user to input service-name, tag-type and then increase the version-type git tag"
  (let* (;; ask user to input a service name
         (service-name (ar/select-service))

         ;; ask user to input a tag type
         (tag-type (ar/select-tag-type))

         ;; compute the next tag
         (next-tag (--> service-name
                        (ar/get-latest-tag it)
                        (ar/get-next-tag service-name it version-type tag-type)))

         ;; set default dir for git to run
         (default-directory AR-CONNECT-PATH))
    (message next-tag)
    (magit-tag next-tag "HEAD")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FMGSUITE specific functions

(let* (;; list of possible locations of FMG.Integrations repo
       (integration-dirs '("~/Projects/fmg.integrations" "c:/Users/me/Projects/fmg.integrations"))

       ;; find the one that exists
       (integration-dir (-find 'file-directory-p integration-dirs)))
  (if integration-dir
      (progn
        ;; Enable the feature
        (defconst FMG-FEATURES-ENABLED t)
        ;; Path to FMG.Integrations repo
        (defconst FMG-INTEGRATIONS-PATH integration-dir))
    (defconst FMG-FEATURES-ENABLED nil)))

(defun fmg/select-service ()
  "Ask for user input for 1 service name"
  (interactive)
  (helm-comp-read "Select service to tag: " '("Integrations" "EmailJournaling")))

(defun fmg/get-latest-tag (service-name)
  "Get the latest tag for one service name"
  (let ((default-directory FMG-INTEGRATIONS-PATH)
        (regex (s-concat service-name "-v")))
    (--> "tag"
         ;; get all git tags for this repo
         (magit-git-lines it)
         ;; filter to the tags that match the service name
         (-filter (lambda (tag) (s-contains? regex tag)) it)
         ;; ;; sort the tags, the oldest first
         (-sort 'tmtxt/compare-version-strings it)
         ;; ;; pick the last tag (the latest one)
         (-last-item it))))

(defun fmg/get-next-tag (service-name tag-name version-type)
  "Get next tag for the input tag-name
   service-name: Integrations
   tag-name: Integrations-v0.0.3
   version-type: major
   => Integrations-v1.0.0"
  (let* ((tag-name-without-version (s-concat service-name "-v"))
         (next-version (-> tag-name
                           (tmtxt/extract-version)
                           (tmtxt/get-next-version version-type))))
    (s-concat tag-name-without-version next-version)))

(defun fmg/increase-version (version-type)
  "Ask user to input service-name and then increase the version-type git tag"
  (let* (;; ask user to input a service name
         (service-name (fmg/select-service))

         ;; compute the next tag
         (next-tag (--> service-name
                        (fmg/get-latest-tag it)
                        (fmg/get-next-tag service-name it version-type)))

         ;; set default dir for git to run
         (default-directory FMG-INTEGRATIONS-PATH))
    (message next-tag)
    (magit-tag next-tag "HEAD")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions for interacting/switching the 2 projects
(defun get-current-project ()
  "Get the current project, connect or fmg.integrations"
  (let* ((get-dir-name (lambda (dir) (-> dir (file-truename) (file-name-as-directory))))
         (current-dir (-> default-directory (file-truename) (file-name-as-directory)))
         (connect-dir (-> AR-CONNECT-PATH (file-truename) (file-name-as-directory)))
         (integrations-dir (-> FMG-INTEGRATIONS-PATH (file-truename) (file-name-as-directory))))
    (cond
     ((string= current-dir connect-dir) 'connect)
     ((string= current-dir integrations-dir) 'integrations)
     t nil)))

(defun fmg/increase-major-tag ()
  "Select a service then increase the major version and add a git tag"
  (interactive)
  (cond
   ((eq (get-current-project) 'connect) (ar/increase-version "major"))
   ((eq (get-current-project) 'integrations) (fmg/increase-version "major"))))

(defun fmg/increase-minor-tag ()
  "Select a service then increase the minor version and add a git tag"
  (interactive)
  (cond
   ((eq (get-current-project) 'connect) (ar/increase-version "minor"))
   ((eq (get-current-project) 'integrations) (fmg/increase-version "minor"))))

(defun fmg/increase-patch-tag ()
  "Select a service then increase the patch version and add a git tag"
  (interactive)
  (cond
   ((eq (get-current-project) 'connect) (ar/increase-version "patch"))
   ((eq (get-current-project) 'integrations) (fmg/increase-version "patch"))))

(provide 'tmtxt-fmgsuite)
