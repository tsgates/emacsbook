%
% Taesoo Kim
%
% title: 모드(Mode)와 동적 바인딩(Dynamic Binding)
%
% abstract: Lisp의 동적 바인딩(Dynamic Binding)이 어떻게 이맥스를 구현하는데 
% abstract: 활용되었는지, 이맥스 안에서 파일을 여는 과정을 통해서 살펴볼 것이다.
%

6장에서는 Lisp의 환경(environment)과 동적 바인딩(dynamic binding)의 특성을
활용하여 주 모드(mode)가 어떻게 구현되었는지 살펴보았다. 이번 장에서는 이맥스
안에서 프로그램을 작성하고, 수정하는 기본적인 방법을 알아 보고, 그 뒤에 숨겨진
이맥스의 내부 구조를 이해해보자.

# 텍스트 입력하기

우리가 같이 작성해볼 프로그램은 시저 암호화(caeser cipher) 알고리즘으로 암호화할
텍스트를 받아 암호화한 코드(cipher)를 출력한다. 이번 장에서는 컴파일형 언어인
C언어로 프로그램을 작성해보자. 먼저 \k{C-x C-f}를 입력하여 "enc.c" 파일을
생성하자. 

아래의 코드를 처음부터 직접 입력해 보자!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
\!{cat enc.c}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

정말로 입력해보았다면 어떤 점이 불편하고, 어떤 기능들을 더 필요한가? 특히 다른
에디터에서 익숙한 기능이 있다면, 하나 하나 적어놓자. 앞으로 배워가는 과정에서,
어떻게 원하는 기능을 찾고, 찾을 수 없다면 스스로 구현하는 방법을 배워나갈
것이다.

# 자동 완성 (dabbrev-completion)

프로그래밍을 하면서 입력하면서 가장 먼저 절실하게? 필요한 기능은 역시 자동
완성이다. 이멕스에서 동적 자동완성 (dabbrev-completion)의 기능이 있는데,
\k{M-/: dabbrev-expand}의 키입력을 통해서 호출 할 수 있다. 

동적 자동완성은 앞문자들이 일치하는 단어들 중 가장 최근에 나타난 (앞선) 단어를
찾아 완성해 준다.

![\n{img} 자동 완성](\s{snap -o emacs-abbrev.png -c -a /tmp/enc.c
    "\"#include <stdio.h>\n#include <string.h>\n#inc\"" C-h m})

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#include <stdio.h>
#include <string.h>
#inc[]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

자 위의 커서에서 \k{M-/}로 자동 완성 시켜보기 바란다.

# 해더 파일 열기 (find-file-at-point)

"stdio.h" 와 "string.h"의 해더 파일은 알겠는데, "err.h"는 무슨 해더 파일일까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#include <stdio.h>
#include <string.h>
#include <[]err.h>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

커서를 해더 파일이름에 놓고, \f{M-x fnid-file-at-point}를 실행해 보자. 시스템에
"err.h" 파일의 위치를 찾아 주는가?

# 간단한 텍스트 수정

자주 쓰이는 텍스트 수정하는 방법들을 알아 본다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
#define rpIME (17)
#define SHIFT (11)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- \k{M-u: upper word}: 단어를 대문자로 변경
- \k{M-l: lower word}: 단어를 소문자로 변경
- \k{M-c: capitalize word}: 단어의 첫 글자를 대문자로, 나머지는 소문자로
- \k{C-t: transpose char}: 앞/뒤 글자를 뒤바꿈
- \k{M-t: transpose word}: 앞/뒤 문자를 뒤바꿈
- \k{C-M-t: transpose sexp}: 앞/뒤 표현식을 뒤바꿈

자 잘못쓰여진 "rpIME"을 "PRIME"으로 변경하려면 아래와 같이 4번의 입력으로 할 수 있다.

- \k{C-f}: 커서를 "r"과 "p"로 놓고
- \k{C-t}: "rp" -> "pr"
- \k{M-b}: 커서를 문자 앞으로 옮기고
- \k{M-u}: 대문자로 단어를 변경

사실, 두 글자 지우고 \{C-d} 다시 "PR"를 입력하는 것 또한 4번의 입력으로
가능하다. 두 글자 지우는 것을 현재 커서에서 "p"까지 지우는 것으로 생각할 수
있다면, \{M-z: zap}을 입력하고 "p"를 입력하면 현재 커서에서 "p"까지 지우고, 다시
"PR"을 입력 할 수 도 있다.

# 주석 쓰기

많은 언어를 접하다 보면 가장 헛갈리는 것 중에 하나가 주석 키워드 이다. 이멕스는
모든 언어들의 주석이 \k{C-;:comment-dwim} 한 키에 바인딩 되어 있다. 즉, 언어의 특징적인
주모드들이 언어에 해당하는 주석 키워드를 이용해 주석을 생성해 준다. 또한 문맥에
따라 다른 주석을 생성하는데 아래와 같은 실험을 해보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
[]
#define []PRIME (17)
#define SHIFT (11)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

두 곳의 커서 위치에서 \k{C-;}를 입력하면 아래와 같이 하나는 현재 줄 전체에, 다른
하나는 오른쪽 공간에 주석을 생성하는 것을 볼 수 있다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
/* [] */
#define PRIME (17)              /* [] */
#define SHIFT (11)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

오? 그러면 주석을 지우고 싶으면? 5장에서 배운 범용 인자 \k{C-u}를 이용해서
주석을 제거해 보자. 위의 커서 위치에서 \k{C-u C-;}를 입력해서 각각의 주석을
제거하자.

또한 범위가 선택되었다면 \k{C-;}는 선택된 범위를 주석 처리 하거나, 만약에 이미
주석이 처리가 되었다면 주석을 제거 할 수도 있다. 범위는 \{C-space:
set-mark-command} 를 이용해서 할 수 있고, 위의 두 define문을 선택하고 \{C-;}를
호출 해 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
/* #define PRIME (17) */
/* #define SHIFT (11) */
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 주석 문서 달기



~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
/**
 * @plain: input text
 * @shift: an integer
 * @prime: a prime number
 */
char* ceaser(const char *plain, int shift, int prime) {
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
                
