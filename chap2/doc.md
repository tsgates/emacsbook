%
% Taesoo Kim
%
% title: 도움말 시스템
%
% abstract: 이멕스가 제공하는 도움말 기능들을 차근차근 알아보고, 도움말
% abstract: 시스템을 활용하여 궁금증에 대한 답을 어떻게 찾을 수 있는지 알아본다.
%

"고기를 잡는 법을 가르쳐라."라는 속담이 있다. 이멕스를 사용하면서 생긴 궁금증을
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

앞에서 정의한 용어들로 message 함수에대한 설명은 이해가 쉽게 되지만, 몇가지
새로운 사용된 용어들을 명확하게 정의하고 넘어가자.

built-in 함수
:   c로 구현된 함수 (primitive 함수).
    이멕스는 c로 구현된 (기본이 되는) built-in 함수들과 lisp으로 구현된
    라이브러리들로 이루어진다. 위의 message 함수는 또한 editfns.c 파일에서
    구현되어 있기 때문에 built-in 함수라고 칭한다.
&rest
:   나머지 함수인자를 리스트로 받겠다는 키워드로 c에서 va_args나, python에서
    * 키워드와 같은 기능을 한다. (참고로 "&optional"는 함수인자를 선택적으로
      받겠다는 키워드)

위의 설명에 따르면 message 함수의 기능적 특성 세가지를 알 수 있다. 

- 메시지를 화면 하단의 영역에 표시한다.
- 메시지를 \*Message\* 버퍼에 기록한다.
- 메시지를 리턴한다.

![\n{img} 도움말 시스템](\s{snap -o help-function.png -c 
   C-x b "\"*scratch*\"" RET 
   "\"(message \\\"%s %s\\\" \\\"hello\\\" (buffer-name))\"" C-j
   C-h f "\"message\"" RET})

또한 message 함수의 인자중 하나인 FORMAT-STRING의 사용법 또한 간단하게 기술되어
있다. (다른 언어와는 달리 Lisp에서는 '-', '?', '!'등은 합법적인 글자이며,
"FORMAT-STRING"는 함수인자의 이름을 나타낸다.)

    The first argument is a format control string, and the rest are data
    to be formatted under control of the string.  See `format' for details.
    
    Note: Use (message "%s" VALUE) to print the value of expressions and
    variables to avoid accidentally interpreting `%' as format specifiers.

위의 주어진 설명을 따라 message 함수를 실행해 보자. \*scratch\* 버퍼로 이동
(\k{C-x b} 이후 \*scratch\*선택) 한 후 아래와 같이 함수를 실행해 본다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(message "%s %s" "hello" (buffer-name))
"hello *scratch*"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 표현식은 message 함수를

- FORMAT-STRING: "%s %s"
- ARGS: ("hello", (buffer-name))

을 인자로 호출한다. (ARGS 함수인자는 나머지 인자들을 묶어 하나의 리스트로
전달한다.)

그럼 buffer-name은 무슨 함수 일까? 이름에서 쉽게 추측가능 하지만, message
함수와 같은 방법으로 찾아보자. (\k{C-h f} 입력 후 buffer-name를 입력한다.) 이제
부터 모르는 함수가 나오면 주어진 방법으로 함수에 대한 설명을 찾아 읽고 넘어가도록
하자.

# 문서 찾기

message 함수의 FORMAT-STRING 인자에는 어떠한 문자열이 입력가능할까? 두번째로 알아
볼 도움말 시스템은 문서를 검색하는 방법이다. 위의 message 함수의 도움말 중 아래의
설명을 보았을 것이다.

    See `format' for details.

친절하게도 message 함수 문서에는 format에 대한 참조 링크를 포함하고 있다. 먼저
커서를 \*Help\* 버퍼로 이동 후 (\k{C-x o:윈도우간 이동}) TAB으로 (커서를 링크가능
키워드로 이동) "format" 위로 커서를 이동시켜 링크를 따라갈 수 있다. 두번째
방법은 모든 문서중 "format"의 키워드를 포함하는 문서들을 찾아보는 방법이다.
문서를 검색하기 위해서 \k{C-h d:문서 찾기}(__h__elp __d__ocument)을 실행 후
"format"을 입력하자.

    format
      Function: Format a string out of a format-string and arguments.
    ...
    %s means print a string argument.  Actually, prints any object, with `princ'.
    %d means print as number in decimal (%o octal, %x hex).
    ...

# 소스 찾기

필자에게 이멕스의 가장 매력적인 기능을 소개해달라고 한다면, 서슴없이 "소스
찾기"기능을 소개하고 싶다. 이멕스는 소위 "리터럴 프로그래밍"이라고하는 방법론을
잘 따르고 있어, 사용자가 이멕스의 하나하나의 기능과 그에 관련된 소스코드를 쉽게
찾아 볼 수 있다. 대부분의 사용자가 이멕스의 내부구조를 뜯어 고치는 작업을 하지
않더라도, 사용자가 문제에 봉착했을때는 언제나 이 기능이 모든 현상들을 가장 잘
설명해 주는 기준이되는 역할을 한다. 내가 쓰고 있는 바로 이 기능이 어떻게
짜여있는지 본다는 것이 얼마나 흥분되는 일인지 모른다!

앞에서 설명한 것과 같이 이멕스는 크게 C로 짜여진 built-in 함수들과 Lisp으로 짜여진
라이브러리들로 구성이 된다. 먼저 C로 짜여진 message 함수가 어떤 소스파일에서
구현되었는지 확인해 보자. message의 도움말을 열고 도움말 버퍼로 이동한 후 "C
source code"의 링크를 따라가 보자.

    message is a built-in function in `C source code'.

"C source code" 링크를 따라가면 소스의 위치를 묻는데 저번 장에서 다운로드 받은
소스코드의 위치 "/usr/src/emacs-[ver]"에 "/src"를 붙여
"/usr/src/emacs-[ver]/src"를 입력하자. 그러면 이멕스는 아래와 같이 해당
소스코드를 찾아 함수의 구현을 보여준다.

![\n{img} C 소스코드 찾기](\s{snap -o lookup-c-source-file.png -c
   C-h f "\"message\"" RET C-x 0 TAB RET
   C-a C-k "\"$HOME/refs/emacs23/src\"" RET C-x 1
   C-h f "\"message\"" RET})

또한 소스코드의 위치를 입력을 한 이후에는 모든 함수의 도움말에 "C source code"의
링크가 해당 파일의 이름으로 표시됨을 알 수 있다.

    message is a built-in function in `editfns.c'.

다음은 \k{C-h C-h}로 실행했던 도움말의 도움말 함수 (help-for-help-internal
함수)의 구현을 살펴 보자. 도움말 함수는 Lisp으로 구현되었으며, 도움말 버퍼에서
"help.el"의 링크를 따라가면 아래와 같이 Lisp으로 구현된 함수의 정의를 보여준다.

![\n{img} Lisp 소스코드 찾기](\s{snap -o lookup-elisp-source-file.png -c
   C-h f "\"help-for-help-internal\"" RET C-x 0 TAB RET C-x 1
   C-h f "\"help-for-help-internal\"" RET})

이제부터는 궁금한 함수 및 명령이 주어지면, 언제든지 함수의 기본적인 (사용)
설명을 찾고 구체적인 구현 또한 찾을 수 있게 되었다. 프로그래머로써 사용하는
프로그램의 소스코드를 손쉽게 찾고, 자유롭게 고칠 수 있다는것이 얼마나 매력적인지
조금씩 알아가게 될 것이다.

# 키에 연결된 함수 찾기

이제는 이멕스가 함수들의 집합임을 정말로 느낄수 있게 되었다. 하지만 우리가 자주
입력하던 \k{C-x b}와 같은 키들은 어떤 함수를 실행하는 걸까? \k{C-h k:키에 연결된
함수 찾기} (__h__elp __k__eys)를 입력한 후 \k{C-x b}를 입력 해보자.


    C-x b runs the command switch-to-buffer, which is an interactive
    built-in function in `buffer.c'.

    It is bound to C-x b, (menu-bar) (buffer) (select-named-buffer).

    (switch-to-buffer BUFFER-OR-NAME &optional NORECORD)
    ...

자 넘어가기 전에 또 한번 새로운 용어들을 살펴 보자.

interactive function
:   사용자에게 직접 노출된 함수/명령들
    Lisp으로 호출 할 수 있는 많은 함수들 중 사용자에게 명시적으로 노출된
    함수들로 command 또는 interactive 함수라고 부른다. 모든 interactive한
    함수(명령)는 \k{M-x:명령 실행}으로 호출가능 하다.

다시 한번 설명하자면, switch-to-buffer라는 함수(명령)은 buffer.c에 정의된 함수로
\k{C-x b}키에 바인드 되어 있고, \m{Buffers->Select Named Buffer}로 호출
가능하다. switch-to-buffer는 interactive한 함수(명령)이기 때문에 \k{M-x}로 호출이
가능하다. \k{M-x}입력후 switch-to-buffer를 입력해보자. \k{C-x b}입력과 같은
프롬프트가 보이는가? 정의된 함수를 호출하는 방법은 위와 같이 매우 다양하다.

 - Lisp에서 직접 호출
 - 키입력을 통해서
 - \k{M-x}를 통해서

그러면 interactive하지 않은 message함수는 어떠한가? \k{M-x} 입력 후 "message"를
입력해도 호출되지 않는다. 우리가 object-oriented 프로그래밍을 할때 명시적으로
사용자가 호출 가능한 함수들을 public 하게 노출시키는 것 처럼 이멕스 역시
사용자가 직접적으로 사용하는 함수들을 interactive라는 형태로 노출하고 있다.

그러면 내가 "a"라는 글자를 입력하면 어떠한 과정을 거쳐 화면에 출력이 될까?
\k{C-h k} 입력후 "a" 자판을 입력해 보자. 

    a runs the command self-insert-command, which is an interactive built-in
    function in `cmds.c'.

    It is bound to many ordinary text characters.

    (self-insert-command N)
    ...

심지어 글자하나를 입력하는것 조차 작은 함수를 호출하는 것이니, 우리가 당연하게
사용하는 하나하나의 기능들은 모두! 특정 함수들을 호출하는 것과 같다. 프로그램을
짤때 하고자하는 일들을 함수의 형태로 잘게 나누고, 목적에 따라 호출 하듯, 이멕스를
쓰고 있는 이 순간 순간이 하고자하는 일을 이루기위해 필요한 함수를 순차적이로
호출하는 것이다. 이러한 함수 형태의 추상화는 이멕스를 사용하는 행위를 그대로
프로그램 가능한 형태로 표현하게 해준다. 

# 용어 찾기

이멕스에 사용되는 용어는 어떻게 찾아 볼 수 있을까? \k{M-x} 입력 이후
search-emacs-glossary를 입력하면 사용되는 모든 용어의 간략한 설명을 찾아 볼 수
있다. 함수의 도움말에서 볼 수 있듯이 \m{Help->Search Document->Emacs
Terminology}에서 호출이 가능한 interactive 함수(명령)이다.

    search-emacs-glossary is an interactive compiled Lisp function in `menu-bar.el'.

    It is bound to (menu-bar) (help-menu) (search-documentation)
    ...

이제는 많이 익숙해져서 도움말에 대한 해석은 뒤로하고 재미있는 Lisp의 용어 하나를
알아보고 넘어가자. Lisp은 interpreter로 해석이 되며 (컴파일 과정이 따로 필요
없는) 하지만 내부적으로는 (실행 속도 향상을 위해) 소스코드를 컴파일해서
바이트코드 (.elc) 형태로 컴파일 한 후 실행한다. 이러한 과정은 스크립트형
언어들에서 일반적으로 볼 수 있는 과정으로, python에서 .pyc로 컴파일 한후 해석하는
것과 같은 이유이다. 위의 설명에서 "compiled Lisp function"는 소스코드가 먼저
바이트코드로 컴파일된 후 로드된 함수를 지칭한다.

# 도움말의 도움말

저자 또한 도움말에 관련된 모든 함수이름, 키바인딩 및 사용법을 기억하지
못한다. 저자 뿐만 아니라 모든 프로그래머들이 그렇듯 매우 편리한 함수가 구현되어
있다. 그렇다면 \k{C-h h} (__h__elp __h__elp)를 입력해 보자.

![\n{img} 도움말의 도움말](\s{snap -o help-help.png -c C-h C-h})

기본적인 도움말 함수에 대한 설명은 이것으로 마치고, 앞으로는 설명하는 과정에서
도움말 함수들이 필요하다면 (그리고 중요하다면) 덧붙혀서 설명하도록하자. 간단히
정리 하자면,

- \k{C-h h}: __h__elp __h__elp (도움말에 대한 도움말)
- \k{C-h d}: __h__elp __d__ocument (도움말 검색)
- \k{C-h k}: __h__elp __k__ey (키 -> 함수)
- \k{C-h f}: __h__elp __f__unction (함수 -> 도움말)

과 같은 도움말에 관련 함수들을 알아 보았다.

