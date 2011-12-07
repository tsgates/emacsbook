%
% Taesoo Kim
%
% title: Emacs 시작하기
%
% abstract: 
% abstract: 
%

두가지 종류의 초기화 파일
1. site-lisp
    /etc/emacs/site-start.d
    --no-site-file
1. .emacs
    ~/.emacs
    --no-init-file

탭과 스페이스
 - hook
 - mode
 - c-mode

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(setq-default indent-tabs-mode nil ; off tab mode
              tab-width        4   ; instead 4 spaces mode
              fill-column      80) ; fill width
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 whitespace-mode
 untabify

mode

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(add-to-list auto-mode-alist
             '(".*/linux-[0-9]\\.[0-9]+\\.[0-9]+*/.*\\.[ch]$" . linux-c-mode))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hook

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(defun check-tab-mode ()
  "Determine tab mode and width"
  (interactive)
  (let ((width 4)
        (mode nil))
    (when (save-excursion
            (goto-char (point-min))
            (search-forward "\t" (min 3000 (point-max)) t))
      (setq width 8)
      (setq mode t))

    (setq indent-tabs-mode mode)
    (setq tab-width width)
    (when mode (message "Set 'tab-mode' with %d" width))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))