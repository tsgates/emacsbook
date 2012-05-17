
앞선 1장~4장에서는 이맥스을 이해하기 위한 가장 기초적인 Lisp의 개념을 알아
보았다. C 언어를 이해했다고 해서 Linux 커널을 모두 이해했다고 할 수 없듯이,
기본적인 Lisp의 특성을 이해했다고 해서 이맥스를 모두 이해했다고 할 수
없다. 하지만 한가지 명확한것은 C 언어를 이해하지 못했다면 커널도 이해할 수
없다는 것이다. 이번 장에서는 Lisp이라는 언어적 특성이 어떻게 이맥스를
디자인하는데 핵심적인 역활을 하고 있는지, 더나아가 구체적인 이맥스의 구조를
언어적 특성에 비추어 알아보도록 할 것이다.

= 이맥스의 시작 과정

이맥스를 실행하면 C언어로 짜여진 \t{built-in} 함수들과, 이를 기반으로하여
Lisp으로 짜여진 표준 라이브러리들을 불러들인다. 이와 더불어 두가지 종류의
Lisp파일을 불러들이는데, 하나는 시스템에 추가적으로 설치되어 모든 사용자가
공유하는 \t{site-file} 라이브러리들이고, 나머지 하나는 개인의 사용자의 Lisp파일인
\t{init-file} 이다. 이 둘은 사용하는 배포판에 따라 다른 위치에 존재하지만,
필자가 사용하는 우분투에서는 아래와 같은 위치에 존재한다. 

- \t{site-file}: /etc/emacs/site-start.el 과 site-start.d/* 파일들
- \t{init-file}: $HOME/.emacs

물론 표준 라이브러리를 제외하고 위의 추가적은 Lisp파일들은 사용자의 편의에 따라
각각 불러들이지 않을 수 있다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ emacs --help | grep 'init-file\|site-file'
--no-init-file, -q          load neither ~/.emacs nor default.el
--no-site-file              do not load site-start.el
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

시스템에 부가적으로 설치된 \t{site-file}은 시스템의 모든 사용자가 공유하는
것으로, 사용자가 `apt-get`과 `yum`등의 Package Management System을 통해 설치한 경우
설치되는 파일들로 필자의 컴퓨터에는 다음과 같은 파일들이 존재한다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ ls -A /etc/emacs/site-start.d
00debian-vars.el          50global.el            50notmuch.el
50anything-el.el          50haskell-mode.el      50psvn.el
50autoconf.el             50js2-mode.el          50python-guppy.el
50auto-install-el.el      50latex-cjk-common.el  50thailatex.el
50dictionaries-common.el  50latex-cjk-thai.el
50emacs-goodies-el.el     50mmm-mode.el
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

일반 Linux의 설정파일과 같이 앞머리 숫자는 불러들일 순서를 나타내며, 각각의
파일들은 해당 모듈들의 시작파일들로 자신만의 초기화 함술를 가지고 있다. 기대와는
달리? 많지 않은 파일들이 있음을 눈치 챘을 것이다. 중요한 이유중에 하나는 필자는
즐겨쓰는 라이브러리들을 최신 버젼으로 관리하고 싶어서인데, 이맥스가 불러들이는
사용자 정의 파일을 통해 간편하게 자신만의 구조를 만들 수 있다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
$ ls ~/.emacs
/home/taesoo/.emacs

$ ls configs/.lisp/
addons/  dev/  info/  org/  os/  soo/  makefile  pkgs  soodir.mk  soo.mk
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

간략하게 구조를 설명하면, 이맥스가 정한 사용자 지정파일(\t{init-file})인
`$HOME/.emacs`(앞으로 `.emacs`)에서 부터 사용되는 운영체제, 터미널 환경에 따라 해당하는
설정파일을 불러들이게 된다. 

일반적으로 이맥스 설정파일을 수정한다고 말할때 우리는 자신의 `.emacs` 파일을
수정하는 것을 의미하며, 특정 패키지를 설치한다고 말할때 우리는 패키지 파일들을
특정 장소에 저장한후 `.emacs` 파일에서 패키를 불러들이는 것을 의미한다.

= 잔소리 ;;

필자가 생각하는 이맥스를 배운다고 하는것은 세가지 관점에서 생각해 볼 수
있다. 첫째는 이맥스의 기반을 이루고 있는 Lisp의 특성들을 이해하는 것이고,
두번째는 이를 근간으로하는 이맥스의 Domain Specific한 특성들을 이해하는
것이다. 마지막으로 자신이 원하는 기능들을 찾아보고, 확장하는 것으로 이맥스를
자신만의 에디터로 만들어가는 것이다.

<!-- refs/linux-3.0-git/test.c -->

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl} -->
<!-- (add-to-list auto-mode-alist -->
<!--              '(".*/linux-[0-9.git-]+/.*\\.[ch]$" . linux-c-mode)) -->
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

=> regex-build



- compilation
  regexp
  child process invocation (async)
- gdb
  process control in emacs
- woman
- c-macro-expand
  sync process invocation

- tips:
  regexp-assign
  c-warn mode
  eshell

=> chap8: python/ruby or haskell (interpreter)
=> chap9: init (permanent)

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
                


<!--  -->
현재 버퍼의 주/부 모드에 관한 정보 및 모드에 대한 키바인딩은 \k{C-h m} (**h**elp
**m**ode) 명령을 통해 살펴볼 수 있다.

C-c .

% snap
% C-h m

regexp
dabbrev-expand
man
ido-mode
comment-dwin
c-macro
c-up/down/next-conditional
cwarn-mode
highlight-parentheses-mode
highlight-current-line-on
flyspell-prog
flymake

# 모드 (Mode)

일반적으로 우리가 특정 파일을 이맥스에서 열었을때 이맥스는 열린 파일에 대한
버퍼를 생성하하고, "버퍼"의 이름에서 알 수 있듯이 사용자는 파일을 직접 수정하는
것 대신 버퍼를 수정하게 된다. 이때 현재 작업하는 버퍼(파일)에 대하여 특별히
유용한 함수들와 키맵(map)의 그룹을 불러 들이게 된다. 예를 들면 C언어로 짜여진
파일을 열었을 경우, C언어에 특화된 함수와 변수(코드 하이라이팅, 주석 처리 등)를
불러들이고, 이러한 함수와 변수를 하나로 모아서 모드(mode)라고 부른다. 즉 C언어로
짜여진 파일을 열면 (".c"나 ".h"의 확장자를 갖는 파일을 열면), 현재 버퍼의
`major-mode`가 `c-mode`가 된다. 버퍼를 기능별로 분류하다 보니, 자연스럽게 하나의
버퍼당 하나의 `major-mode`를 갖게 되며, 현재 버퍼의 모드는 `major-mode`의 변수에
기록이 된다. \f{C-h v: 변수 도움말}을 현재 버퍼의 주모드가 무엇인지 확인해 보도록
하자.


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
                
