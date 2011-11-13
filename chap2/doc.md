%
% Taesoo Kim
%
% title: 도움말 시스템
%
% abstract: 이멕스가 제공하는 도움말 기능들을 차근 차근 알아보고, 도움말 
% abstract: 시스템을 활용하여 궁금증에 대한 답을 어떻게 찾을 수 있는지 알아본다.
%

"고기를 잡는법을 가르쳐라."라는 속담이 있다. 이멕스를 사용하면서 생긴 궁금증을
어떻게 해결할까? 이멕스는 다양한 방법으로 사용자들을 도움을 줄 수 있도록 구성되어
있다. 이번 장을 통해서 이멕스가 제공하는 도움말 시스템을 직접적으로 활용하기 위한
"질문하는 법"을 알아보도록 한다.

# 함수 찾기

모든 지식 전달이 그렇듯, 이멕스역시 가르치기위해 함수 하나 하나, 기능 하나 하나를
설명하는것은 필자의 시간낭비일 뿐 아니라 독자의 시간낭비이기도 하다. 지난 장에서
Lisp의 message함수를 호출해 보았다. 그럼 우리가 호출할 수 있는 함수는 무엇들이
있으며? 함수에 대한 문서(사용설명서)는 어떻게 찾을까?

- \k{C-h f:함수 설명서}: 함수 -> 문서
- \k{C-h d:함수 문서 검색}: 검색어 -> 함수

먼저 \k{C-h f} ("__h__elp __f__unction"의 앞머리 글자)은 함수이름이 주어졌을때
해당 문서를 보여준다. 한번 \k{C-h f}를 실행하고 message함수를 검색해
보자. (언제나 자동 완성이 됨을 알아두자.)

    message is a built-in function in `editfns.c'.
    
    (message FORMAT-STRING &rest ARGS)
    
    Display a message at the bottom of the screen.
    The message also goes into the `*Messages*' buffer.
    (In keyboard macros, that's all it does.)
    Return the message.
    ...

![\n{img} 도움말 시스템](\s{snap -o help-function.png -c 
   C-h f "\"message\"" RET})

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

# 문서 찾기

message함수의 format-string은 어떠한 스팩을 가지고 있을까? 두번째로 알아 볼
도움말 시스템은 문서를 검색하는 방법이다. 위의 message 함수의 도움말 중 아래의
설명을 보았을 것이다. 

    See `format' for details.

첫번째 방법은 커서를 \*Help\* 버퍼로 이동 (\k{C-o:버퍼간 이동}) 후 TAB으로
밑줄쳐진 format 위로 커서를 이동한 후 리턴키를 입력하는 방법이다 (해보자.). 두번째
방법은 모든 문서중 format의 키워드를 포함하는 문서들을 나열하는 방법이다.
\k{C-h d:문서 찾기} (__h__elp __d__ocument)이후 format을 입력하자. 

    format
      Function: Format a string out of a format-string and arguments.
    ...
    %s means print a string argument.  Actually, prints any object, with `princ'.
    %d means print as number in decimal (%o octal, %x hex).
    ...

# TODO. 소스 찾기

1. message 함수 

    message is a built-in function in `C source code'.

vs

    message is a built-in function in `editfns.c'.

2. pick one

# 키에 연결된 함수 찾기

우리가 자주 입력하던 \k{C-x b} 키는 무슨 함수를 실행하는 걸까?
\k{C-h k:키에 연결된 함수 찾기} (__h__elp __k__eys)를 입력한 후 \k{C-x b}를 입력
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

# 용어 찾기

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

# 도움말의 도움말

저자 또한 도움말에 관련된 모든 함수이름 키바인딩을 기억하지 못한다. 저자 뿐만
아니라 모든 프로그래머들이 그렇듯 매우 편리한 함수가 구현되어 있다. 그렇다면
\k{C-h h} (__h__elp __h__elp)를 입력해 보자.

기본적인 도움말 함수에 대한 설명은 이것으로 마치고, 앞으로는 설명하는 과정에서
도움말 함수들이 필요하다면 (그리고 중요하다면) 덫붙혀서 설명하도록하자. 간단히
정리 하자면,

- \k{C-h h}: __h__elp __h__elp
- \k{C-h k}: __h__elp __k__ey
- \k{C-h f}: __h__elp __f__unction
- \k{C-h d}: __h__elp __d__ocument

과 같은 도움말에 관련 함수들을 알아 보았다.

