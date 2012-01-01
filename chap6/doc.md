%
% Taesoo Kim
%
% title: 모드(Mode)와 편집
%
% abstract: 
% abstract: 
%

5장에서는 이맥스 내부의 키맵(Keymap) 구조와 기본적인 커서 이동법에 대해서 알아
보았다. 이번 장에서는 이맥스 안에서 프로그램을 작성하고 수정하는 기본적인 방법을
알아 보고, 그 뒤에 숨겨진 이맥스의 내부 구조를 이해해 보자.

# 파일 열기

앞으로 같이 작성해볼 프로그램은, 시져 암호화(ceaser cipher) 프로그램으로 암호화할
텍스트를 인자로 받아, 암호화한 텍스트를 출력해 준다. 먼저 C언어로 프로그래밍
해보자. 이맥스를 실행하고, 파일을 열기위해 \k{C-x C-f: 파일 열기} 을 입력하여
\f{find-file}을 실행하자.

![\n{img} 파일 열기](\s{snap -o emacs-find-file.png -c C-x f TAB})

미니버퍼에 나타난 프롬프트가 보이는가? 미니버퍼에서 (관행적으로) 제공하는
일반적인 3가지 기능은 아래와 같다.

- \k{M-n: next-history-element}: 다음 입력 히스로리
- \k{M-p: previous-history-element}: 이전 입력 히스토리
- \k{TAB: minibuffer-complete}: 자동완성

\k{C-x f}입력 후 \k{TAB}을 입력해 보면, 현재 폴더에 있는 파일들의 리스트를
\*Completions\* 버퍼에서 확인해 볼 수 있고, 부분적인 파일이름 입력후 자동
완성됨을 확인해 볼 수 있다.

자 정말로 파일을 열어 볼까? 현재 폴더에 파일을 만들기 위해 "enc.c"을 입력해보자.

# 버퍼 상태바

% basic info
% major/minor
% click -> toggle status

# 모드

% auto-mode-alist
% snap
% C-h m

# 텍스트 입력하기

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
\!{cat enc.c}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                