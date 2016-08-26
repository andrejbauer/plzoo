;; Support for compiling in subdirectories from Emacs. Adapted from Coq source.
((nil
  . ((eval
      . (progn
          ;; root directory (ending with slash)
          (let ((plzoo-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (plzoo-project-find-file
                 (and (boundp 'plzoo-project-find-file) plzoo-project-find-file)))

            ;; plzoo tags file
            (when plzoo-root-directory
              (setq tags-file-name (concat plzoo-root-directory "TAGS"))
              (add-to-list 'compilation-search-path plzoo-root-directory)
              ;; Setting the compilation directory to plzoo root. This is
              ;; mutually exclusive with the setting of default-directory
              ;; below.
              (if (not plzoo-project-find-file)
                  (setq compile-command (concat "make -C " plzoo-root-directory)))
              )
            (setq plzoo-executable (concat plzoo-root-directory "all")))))))
)
