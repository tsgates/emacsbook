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
                
