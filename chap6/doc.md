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

앞으로 같이 작성해볼 프로그램은 시져 암호화(caeser cipher) 알고리즘으로 암호화할
텍스트를 인자로 받아 암호화한 텍스트를 출력한다. 먼저 가장 일반적으로 알려진
C언어로 프로그래밍 해보자. 이맥스를 실행하고, 파일을 열기위해 \k{C-x C-f:
파일 열기}을 입력하여 \f{find-file}을 실행하자.

![\n{img} 파일 열기](\s{snap -o emacs-find-file.png -s 80x10 -c
    C-x C-f TAB TAB C-x o C-x 0})

미니버퍼에 나타난 프롬프트가 보이는가? 미니버퍼에서 3가지 기능을 제공하는데,
각각을 나열하면 아래와 같다.

- \k{M-n: next-history-element}: 다음 입력 히스로리
- \k{M-p: previous-history-element}: 이전 입력 히스토리
- \k{TAB: minibuffer-complete}: 자동완성

\k{C-x f}입력 후 \k{TAB}을 입력해 보면, 현재 폴더에 있는 파일들의 리스트를
\*Completions\* 버퍼에서 확인해 볼 수 있고, 부분적인 파일이름 입력후 자동
완성됨을 확인해 볼 수 있다.

또한 이전에 입력했던 히스토리를 \k{M-n}(**n**ext)와 \k{M-p}(**p**revious)의
키입력으로 찾아 볼 수 있다. 이맥스에서 사용자의 입력은 \f{completing-read}를
기본적으로 사용하여 구현되어 있으며, 함수의 설명을 찾아 보면 아래와 같다.

    completing-read is a built-in function in `C source code'.
    
    (completing-read PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH
    INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)
    ....

몇몇 인자들을 사용자 입장에서 살펴보면, 사용자는 `INITIAL-INPUT`이 기본적으로
입력되어 있는 입력을 `PROMPT`를 보게되고, `TAB`으로 자동완성을 하려고하면
`COLLECTION`에 있는 인자를 찾아 자동완성해준다. 또한 이전에 입력된 히스토리는
`HIST`에 기록되어 있으며 \k{M-n}과 \k{M-p}로 찾아 입력할 수 있다. \f{find-file}의
입장에서 간략히 살펴보면,

PROMT
:    "Find File" 프롬프트
INITIAL-INPUT
:    현재 디렉토리
COLLECTION
:    현재 디렉토리에 있는 파일들
HIST
:    \v{file-name-history} 변수로 사용자가 열어본 파일들을 기록

사용자의 입력은 항상 이와 같은 방법으로 이루어 지지만, 파일을 읽는다거나
디렉토리를 읽는것 같은 일반적인 일들은, \f{read-file-name}과
\f{read-directory-name}과 같은 특화된 함수로 더욱 쉽게 호출이 가능하다. 

자 만약 특정 파일이나, 특정 패턴을 버전컨트롤 시스템에서 제외하고 싶다고하면,
이를 입력받기위해서 아래와 같은 코드를 작성할 수 있다. 기본적으로 자주 입력되는
패턴들은 `COLLECTION`인자로, 현제버퍼의 파일이름을 `INITIAL-INPUT`으로, 사용자가
열어본 파일 히스트로를 `HIST`인자로 전달할 수 있겠다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(completing-read "Add to .gitignore >> "           ; prompt
                 '(".o", "*~", ".#*") nil nil      ; list of completions
                 (buffer-file-name)                ; initial input
                 'file-name-history)               ; reuse file-name-history
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

이제 어떻게 사용자로부터 파일이름을 입력받았는지 이해했으니, 정말로 파일을
입력하고 생성해볼까? \k{C-x C-f}를 입력하고 앞으로 작성할 파일이름인 "enc.c"을
입력해보자.

# 버퍼 상태바

새로운 파일을 생성했다면 아래와 같은 빈 버퍼를 볼 수 있다. 

![\n{img} 파일 열기](\s{snap -o emacs-find-file-enc.png -s 80x10 
    -c -a ~/tmp/enc.c C-x 1})

직관적인 (그래서 밋밋한) 메뉴와 툴바를 제외하면, 유일하게 호기심을 자극하는 것은
상태바 뿐이다. 상태바위에 일단 마우스로 가져가 올려놔 볼까? 툴팁에 상세하게
어떠한 상태를 나타내는 표시인지, 또 마우스의 어떠한 버튼을 클릭하면 어떠한
동작을 하는지 상세하게 나타난다.



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
                
