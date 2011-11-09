%
% Taesoo Kim
%
% 이멕스 설치 및 실행
%
% 시작하기에 앞서 이멕스를 설치하고, 작업환경을 갖춘다. 또한 설치된 이멕스의
% 디렉토리 구조를 개관적으로 살펴보고, 앞으로 사용할 용어를 정의한다.
% 

# 설치하기

필자가 가장 즐겨쓰고 있는 우분투 배포판을 기준으로 설명하도록 하겠다. 우분투
(데비안 계열) 배포판에서는 아래와 같이 최신 버젼의 이멕스를 설치할 수 있다.

    $ sudo apt-get install emacs23 emacs23-el emacs-goodies-el

이멕스는 오랜기간동안 사람들이 선호하는 기호에 따라 여러종류의 이멕스가 파생되어
개발이 되고 있다. 그 중 앞으로 초점을 맞추어 살펴보게 될 이멕스는 리차드스톨만이
처음 개발하고 일관된 철학으로 지금까지도 많은 사람들이 활발하게 사용하고
개발하고있는 그누이멕스이다.

간략하게 우리가 무엇을 설치하고 있는지 알아보면,

emacs23
:   이멕스 바이너리 및 컴파일된 기본 리습파일
emacs23-el
:   기본 리습 소스파일
emacs-goodies-el
:   추가적인 리습 라이브러리

한가지더, 이멕스의 소스파일을 추가적으로 다운받아 놓자.

    $ apt-get source emacs23
    
현재 디렉토리에 다운받아진 emacs23-[ver] 디렉토리를 ([ver]은 "23.2+1"와 같은
배포판에 종속적인 문자열) 원하는 곳으로 (예를 들면, /usr/src/emacs-[ver]) 옮겨
놓도록하자. 우리가 관심있게 살펴볼 소스코드들은 "emacs23-[ver]/src" 디렉토리에
모여 있다. 각각의 관심있는 소스파일들은 앞으로 필요할때마다 하나씩 천천히
알아보도록하자.

마지막으로, 우리는 무엇을 설치했는가?

/usr/share/emacs/[ver]/lisp
:   컴파일된 리습파일(.elc)과 컴파일된 리습파일(.el.gz)이 모여있는 디렉토리
/usr/share/emacs/[ver]/site-lisp
:   추가적인 리습라이브러리(예를 들면, emacs-goodies-el)이 모여있는 디렉토리
/usr/lib/emacs/[ver]/[arch]/
:   이멕스에서 내부적으로 사용되는 실행 파일들(예를 들면, hexl)이 모여있는 디렉토리
/usr/bin/emacs[ver]
:   이멕스 실행파일

기본적인 작업환경이 이해가됬다면 본격적으로 이멕스에 대해 알아보자.

# 시작하기

흥미 진지한 역사, 철학, 종교(?)적인 이야기는 아쉽지만 뒤로하고, 설치한 이멕스를
다음과 같은 명령으로 실행해 보자.

    emacs

실행된 GUI 버전의 이멕스를 종료하기전에 \m{File->Quit} 메뉴를 살펴보면 \k{C-x
C-c:종료}의 단축키가 적혀있는걸 볼 수 있다. 이멕스에서는 단축키를 아래와 같은 규칙을
가지고 나타낸다.

    C-[char]
    M-[char]

컨트롤(Ctrl)키는 알파벳 **C**로 메타(Meta)키는 **M**으로 나타내는데, 메타키는
일반 키보드의 Esc키와 Alt키를 의미한다. 즉 \k{C-x}라고 하면 컨트롤키를 *누른
상태*에서 키보드의 **x**자판을 누르는 것을 의미하고, \k{C-x C-c}라고 하면 (반복
하자면) 컨트롤키를 *누른 상태*에서 *x*를 누르고 다시 컨트롤키를 *누른 상태*에서
*c*를 누르는것을 의미한다.

다시 이멕스를 실행하자. 

    emacs -nw
    
필자는 종종 간단한 설정파일을 수정하기위해서 위의 명령으로 터미널에서 이멕스를
실행하곤한다. (nw는 _n_o-_w_indow의 약자로 쉽게 기억할수 있다.) 터미널에 보이는
이멕스는 처음에 실행시킨 GUI 이멕스와 흡사하게 생긴것을 확인 할 수 있다. 하지만,
터미널에서 실행되고 있는 이멕스를 종료하려고하면 직관적인 방법으로는 종료할 수
없음을 쉽게 알 수 있을것이다. 여느 이멕스 사용자와 마찬가지로, 필자는 또한 처음
이멕스를 실행한후 종료하지 못해 당황한적이있다. 그후 오랫동안 이멕스를 배우려고
하지 않았기 때문에 독자들에게 가장먼저 종료하는 방법을 알려주고 싶었다.

앞으로 강좌를 계속하기전 두가지 *반드시* 해야되는 숙제가 있다. 

 - 이멕스 가이드투어 훑어보기 \l{http://www.gnu.org/software/emacs/tour/}
 - 이멕스 튜토리얼 \m{Help->Tutorial} 따라하기
 - (선택) 이멕스 논문 훑어보기

# 용어 통일하기

TODO. "이멕스" 단어 설명

확장성이 뛰어난, 그래서 프로그램가능한 에디터는 어떻게 만들수 있을까? 필자와 같이
조그만한 에디터를 디자인해보자. 필자가 생각하는 에디터는 키보드입력을 처리하는
부분과 이를 출력하는 부분, 크게 두개의 부분으로 구성이 된다. 그러면 에디터를
어떻게 C언어로 구현할 수 있을까? 키보드 입력부분에서 OS를 통해 자판코드를
읽어오면, 룩업테이블(코드 -> 함수)을 찾아 해당 함수를 호출하게 할것이다. 호출된
함수에서는 나의 입력을 처리하고 화면에 그 결과를 출력해 주면 될것이다.

이렇게 디자인한 에디터가 확장성이 있을까? 만약 에디터가 사용하는 프로그램언어에
따라 다른 기능을 제공하고 싶으면 어떻게 룩업테이블을 디자인하면 좋을까? 아마도
에디터가 지원하는 언어들에 해당하는 각각의 룩업테이블을 만들고 사용하고 있는
언어에 따라 키보드 입력부분에서 해당 룩업테이블을 사용하면 될것이다. 이멕스는
필자가 설명하고 있는 하나하나의 작업들을 모두 사용자에게 이멕스리습이라는 언어로
노출하고 있다. 

앞으로 사용할 용어들을 정리하면,

<!-- TODO. 상태바, 메뉴 ... -->
 - 버퍼: 기본적인 편집의 단위 (파일 <-> 버퍼 <-> 이멕스)
    버퍼는 이멕스가 편집을 하는 기본 유닛이고, 파일은 버퍼라는 형태로 이멕스
    안에서 처리된다. 사용자는 여러개의 버퍼를 가질수 있지만, 오직 하나의 버퍼만
    수정할 수 있다. 
 - 윈도우: 출력 단위 (버퍼 <-> 윈도우 <-> 사용자)
    버퍼는 윈도우를 통해야만 사용자에게 보여질 수 있다. 다른 두개의 윈도우가
    하나의 버퍼를 출력할수있다. 하지만 하나의 윈도우는 반드시 하나의 버퍼만을
    나타낼 수 있다.
 - 프레임: 윈도우의 배열
    프레임은 여러 윈도우를 갖을 수 있고, 이들의 배열을 결정한다. 즉 프레임의
    윈도우 배열을 수정하여 윈도우를 수직/수평으로 나열할 수 있다.
 - 미니버퍼: 상용자 입력 및 상태바
    복잡한 사용자의 입력을 받고, 입력 상태(에코)를 나타내주는 특별한 윈도우이다.

앞으로 사용할 기본적인 용어들이다. 하지만, 궁금한 용어가 나타난다면
\m{Help->Search Document->Emacs Terminology}를 통해서 꼭 확인해 보기 바란다.

# Lisp 맛보기

이멕스를 다시 실행하고 \k{C-x b:버퍼변경}을 입력해보자. 다시한번, \k{C-x b}는
컨트롤키를 *누른상태*에서 **c**을 누르고, (컨트롤키를 *때고*) **b**을 누르면
된다. \k{C-x b}를 실행하면 "Switch to buffer" 프롬프트가 미니버퍼에 나타난것을
볼 수 있다. 여기서 Tab키를 눌러보자. 화면에 아래와 같은 버퍼 목록을 볼 수 있다.

    *Messages*
    *scratch*

이멕스에서는 관행적으로 파일로 매핑되어 있지 않는 특별한 버퍼이름 앞에 \*를 붙여
관리 한다. 기본적으로 이멕스는 위에 나열된 두개의 특별한 버퍼를 생성한다.
Message 버퍼는 이멕스가 출력했던 메시지들이 기록된며, Scratch (스크레치북의
스크레치) 버퍼는 저장하지 않을 목적으로 노트하거나, (더욱 중요하게) 이멕스함수를 
evaluate하기 위한 목적으로 사용된다.

자 그럼 이멕스리습을 맛보기위해서 scratch 버퍼를 열어 보자. \k{C-x b}후
\*scr[TAB]으로 자동완성해서 scratch 버퍼를 선택하자. 그리고 첫 lisp 프로그램을
만들어 보자.

    (message "hello world")

위의 표현식을 evaluate하기 위해서 마지막 괄호 다음에 커서를 두고 \k{C-j}를
눌러보자.

    (message "hello world")
    "hello world"

표현식 아래 결과(리턴)값과 미니버퍼 (스크린 아래)에 "hello world"가 출력됨을 알
수 있다. 그럼 위의 표현식이 어떤 의미를 하는지 알아보자.

    +--- 표현식의 시작
    v
    (message "hello world")
      함수       문자열     ^
                          |
           표현식의 끝 -----+

우리가 쉽게 추측해 볼 수 있듯, 위의 표현식은 message라는 함수를 "hello world"의
문자열을 인자로하여 호출한 것이다. 사실 이것이 lisp의
syntax의전부이다. lisp에서는 일반 imperative 언어에서 특별하게 여겨지는 표현식,
제어문 등 또한 하나의 함수 호출의 형태를 띄고있다. 그러면 우리가 호출할 수 있는
함수는 무엇이며 어떻게 사용할 수 있을까?

# 도움말 시스템

어떻게 궁금증을 해결할까? 이멕스는 다양한 방법으로 사용자들을 도울수 있도록
구성되어 있다. 이번 장을 통해서 어떻게 이멕스가 기본적으로 제공하는 도움말
시스템을 활용하는지 알아보자. 

## 함수 찾기

이멕스를 (사실 모든 학문을) 가르쳐준다고 해서 함수 하나 하나 기능 하나 하나를
설명하는것은 저자의 시간낭비일 뿐 아니라 독자의 시간낭비이기도 하다. 그럼 우리가
호출할 수 있는 함수는 무엇들이며 해당 문서를 어떻게 찾는지 먼저 알아보자.

    \k{C-h f:함수 설명서}: 함수 -> 문서
    \k{C-h d:함수 문서 검색}: 검색어 -> 함수

먼저 \k{C-h f} (_h_elp _f_unction)은 함수이름이 주어졌을때 해당 문서를
보여준다. 한번 \k{C-h f}를 실행하고 message함수를 검색해 보자. (언제나 자동
완성이 됨을 알아두자.)

    message is a built-in function in `editfns.c'.
    
    (message FORMAT-STRING &rest ARGS)
    
    Display a message at the bottom of the screen.
    The message also goes into the `*Messages*' buffer.
    (In keyboard macros, that's all it does.)
    Return the message.
    ...

대부분의 설명은 앞에서 배운 용어들로 이해가 쉽게 되지만, 몇가지 새로운 용어들이
있다.

 - built-in 함수: c로 구현된 함수 (primitive 함수)
   이멕스는 c로 구현된 (기본이 되는) built-in 함수들과 lisp으로 구현된
   라이브러리들로 이루어진다. 위의 message 함수는 또한 editfns.c 파일에서
   구현된 함수를 찾아 볼 수 있다. 
 - &rest: 나머지 함수인자를 리스트로 받겠다는 키워드
   c에서 va_args나 ...와 같은 키워드 또는 python에서 * 키워드
   (&optional: 함수인자는 옵션)

위의 설명에서 message의 특성 세가지를 알 수 있다. 

 - 메시지를 화면 하단의 영역에 표시한다.
 - 메시지를 \*Message\* 버퍼에 기록한다.
 - 메시지를 리턴한다.

또한 도움말의 하단에 FORMAT-STRING 인자의 사용법 또한 간단하게 기술되어 있다.

    The first argument is a format control string, and the rest are data
    to be formatted under control of the string.  See `format' for details.
    
    Note: Use (message "%s" VALUE) to print the value of expressions and
    variables to avoid accidentally interpreting `%' as format specifiers.

먼저 message함수를 주어진 설명에 따라 실행해 보자. \*scratch\* 버퍼로 이동 (\k{C-x b})
한 후 아래와 같이 함수를 실행해 본다.

    (message "%s %s" "hello" (buffer-name))
    "hello *scratch*"

위의 표현식은 message 함수를
  - "%s %s" (format-string)
  - "hello" 문자열
  - buffer-name 함수의 호출 결과값
을 인자로 호출한다.

자 그럼 buffer-name은 무슨 함수 일까? message와 같은 방법으로 \k{C-h f} 실행 후
buffer-name의 문서를 찾아보자. 이제 부터 모르는 함수가 나오면 (또한 궁금하다면)
주어진 방법으로 함수에 대한 설명을 찾아 읽고 넘어가도록 하자.

## 문서 찾기

message함수의 format-string은 어떠한 스팩을 가지고 있을까? 두번째로 알아 볼
도움말 시스템은 문서를 검색하는 방법이다. 위의 message 함수의 도움말 중 아래의
설명을 보았을 것이다. 

    See `format' for details.

첫번째 방법은 커서를 \*Help\* 버퍼로 이동 (\k{C-o:버퍼간 이동}) 후 TAB으로
밑줄쳐진 format 위로 커서를 이동한 후 리턴키를 입력하는 방법이다 (해보자.). 두번째
방법은 모든 문서중 format의 키워드를 포함하는 문서들을 나열하는 방법이다.
\k{C-h d:문서 찾기} (_h_elp _d_ocument)이후 format을 입력하자. 

    format
      Function: Format a string out of a format-string and arguments.
    ...
    %s means print a string argument.  Actually, prints any object, with `princ'.
    %d means print as number in decimal (%o octal, %x hex).
    ...

## TODO. 소스 찾기

1. message 함수 

    message is a built-in function in `C source code'.

vs

    message is a built-in function in `editfns.c'.

2. pick one

## 키에 연결된 함수 찾기

우리가 자주 입력하던 \k{C-x b} 키는 무슨 함수를 실행하는 걸까?
\k{C-h k:키에 연결된 함수 찾기} (_h_elp _k_eys)를 입력한 후 \k{C-x b}를 입력
해보자. 

    C-x b runs the command switch-to-buffer, which is an interactive
    built-in function in `buffer.c'.

    It is bound to C-x b, <menu-bar> <buffer> <select-named-buffer>.

    (switch-to-buffer BUFFER-OR-NAME &optional NORECORD)
    ...

자 넘어가기 전에 또 한번 새로운 용어들을 살펴 보자.

 - command & interactive: 사용자에게 직접적으로 노출된 함수들
   Lisp으로 호출 할 수 있는 많은 함수들 중 사용자에게 명시적으로 노출된
   함수들로 command 또는 interactive 함수라고 호칭한다. 모든 interactive한
   함수(명령)는 \k{M-x:명령 실행}으로 실행가능 하다.

다시 한번 설명하자면, switch-to-buffer라는 함수(명령)은 buffer.c에 정의된 함수로
\k{C-x b}에 바인드 되어 있고, \m{Buffers->Select Named Buffer}로 호출
가능하다. switch-to-buffer는 interactive한 함수(명령)이기 때문에 \k{M-x}로
호출이 가능하다. \k{M-x}입력후 switch-to-buffer를 입력해보자. \k{C-x b}입력과
같은 프롬프트가 보이는가? 정의된 함수를 호출하는 방법은 위와 같이 매우
다양하다.

 - Lisp에서 직접 호출
 - 키입력을 통해서
 - \k{M-x}를 통해서

그러면 interactive하지 않은 message함수는 어떠한가? \k{M-x} 입력 후 message를
입력해도 호출되지 않는다. 우리가 object oriented 프로그래밍을 할때 명시적으로
사용자가 호풀 가능한 함수들을 public (공용)하게 노출 시키는 것 처럼 이멕스 역시
사용자가 직접적으로 사용하는 함수들을 interactive라는 형식으로 노출 하고 있다.

## 용어 찾기

이멕스에 사용되는 용어는 어떻게 찾아 볼 수 있을까? \k{M-x} 입력 이후
search-emacs-glossary를 입력하면 사용되는 모든 용어의 간략한 설명을 볼 수
있다. 함수의 도움말에서 볼 수 있듯이 \m{Help->Search Document->Emacs
Terminology}에서 호출이 가능하고 interactive 함수(명령)이다.

    search-emacs-glossary is an interactive compiled Lisp function.

    It is bound to <menu-bar> <help-menu> <search-documentation>
    <emacs-terminology>.
    ...

이제는 많이 익숙해져서 도움말에 대한 해석은 뒤로하고 재미있는 lisp의 용어 하나를
알아보고 넘어가자. Lisp은 interpreter로 해석이 되며 (컴파일 과정이 따로 필요
없는) 하지만 내부적으로는 (실행 속도 향상을 위해) 소스코드를 컴파일해서
바이트코드 (.elc) 형태로 컴파일 한 후 실행한다. 이러한 과정은 스크립트형
언어들에서 일반적으로 볼 수 있는 과정으로, python에서 .pyc로 컴파일 한후
해석하는 것과 같은 이유이다.

## 도움말의 도움말

저자 또한 도움말에 관련된 모든 함수이름 키바인딩을 기억하지 못한다. 저자 뿐만
아니라 모든 프로그래머들이 그렇듯 매우 편리한 함수가 구현되어 있다. 그렇다면
\k{C-h h} (_h_elp _h_elp)를 입력해 보자.

기본적인 도움말 함수에 대한 설명은 이것으로 마치고, 앞으로는 설명하는 과정에서
도움말 함수들이 필요하다면 (그리고 중요하다면) 덫붙혀서 설명하도록하자. 간단히
정리 하자면,

- \k{C-h h}: _h_elp _h_elp
- \k{C-h k}: _h_elp _k_ey
- \k{C-h f}: _h_elp _f_unction
- \k{C-h d}: _h_elp _d_ocument

과 같은 도움말에 관련 함수들을 알아 보았다.

