%
% Taesoo Kim
%
% title: 모드(Mode)와 편집
%
% abstract: 
% abstract: 
%

 - with a c example
 - editing/searching
 
 - modifying global-map (C-z -> undo or C-q -> kill-buffer)
 - explain init process

 => next chapter (tools, compile ...)
 => windowing
 => modifying local-map
 

ido 소개
-> permenant?
-> init file

기본틀 -> 설명?
- basic option feilds
- linux kernel mode?

모드
- .bashrc 파일 열어보기
- 개념
- 키 -> 함수
- .c

=> so a way to recognize this: auto-mode-alist

refs/linux-3.0-git/test.c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(add-to-list auto-mode-alist
             '(".*/linux-[0-9.git-]+/.*\\.[ch]$" . linux-c-mode))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

=> regex-build

응용예) 탭과 스페이스

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(setq-default indent-tabs-mode nil ; off tab mode
              tab-width        4   ; instead 4 spaces mode
              fill-column      80) ; fill width
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

show:
 whitespace-mode
 untabify

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
                
                


- jumping

    C-u C-SPC 
    M-g g (goto-line)
    M-g n (next-error)
    M-g p (previous-error)
    
- search

    C-s isearch-forward
    C-r isearch-backward

    C-M-s isearch-forward-regexp
    C-M-r isearch-backward-regexp
    
- editing

    DEL delete-backward-char
    M-DEL backward-kill-word
    C-DEL delete-kill-word
    C-d delete-char
    M-d kill-word
    M-k kill-line
    C-M-k kill-sexp
    M-z zap-to-char
    C-t 
    M-t 
    C-M-t
    C-o
    C-x C-o
    C-j
    C-space

- windowing

    C-x 1
    C-x 2
    C-x 3
    C-x 5
    C-M-v: scroll-other-window-up
    C-M-S-v: scroll-other-window-down
    
- register

=> end with how to modify global-map and permanently change it
                