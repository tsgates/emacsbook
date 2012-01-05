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

# 버퍼 상태바 (Mode Line)

새로운 파일을 생성했다면 아래와 같은 빈 버퍼를 볼 수 있다. 

![\n{img} 파일 열기](\s{snap -o emacs-find-file-enc.png -s 80x10 
    -c -a ~/tmp/enc.c C-x 1})

직관적인 (그래서 밋밋한) 메뉴와 툴바를 제외하면, 유일하게 호기심을 자극하는 것은
상태바 뿐이다. 상태바위에 일단 마우스로 가져가 올려놔 볼까? 툴팁에 상세하게
어떠한 상태를 나타내는 표시인지, 또 마우스의 어떠한 버튼을 클릭하면 어떠한
동작을 하는지 상세하게 나타난다.

     +- mode-line-mule-info (입력방법, U:Unicode)
     | +- mode-line-modified (버퍼 쓰기/읽기 가능 상태)
     | | +- mode-line-remote (원격/로컬 파일)
     | | | +- mode-line-frame-identification (버퍼이름)
     | | | |                               +- global-mode-string (주/부모드)
     v v v v                               v               
    -----------------------------------------------------------------------
    -U:--- enc.c           All L1          (C/l Abbrev)
    =======================================================================
    ^ ^ ^                  ^    ^            
    | | |                  |    +- mode-line-position
    | | |                  +- mode-line-buffer-identification (위치, 줄)
    | | +- mode-line-modified (버퍼 수정 상태)
    | +- end of line style (unix)
    +- full memory

엄청 복잡해 보이지만, 입력방식, 버퍼의 상태, 커서의 위치, 주모드/부모드등을
나타내고 있다. 이맥스에서 이러한 상태바를 구성할지 \v{mode-line-format:
상태바형식} 변수를 통해서 정의 하고 있고, 한순한 리스트의 형태를 하고 있다. 현재
상태바는 어떠한 값을 가지고 있는지 \k{C-h v}를 통해서 찾아 보자.

    mode-line-format is a variable defined in `C source code'.
    Its value is shown below.
    
      Automatically becomes buffer-local when set in any fashion.
      This variable is potentially risky when used as a file local variable.
    
    Documentation:
    Template for displaying mode line for current buffer.
    ...
    For a symbol,  ...
    For a list of the form `(:eval FORM)', ...
    For a list of the form `(:propertize ELT PROPS...)', ...
    A string is printed verbatim in the mode line except for %-constructs:
      ...
      %b -- print buffer name.      %f -- print visited file name.
      %F -- print frame name.
      ...
      
아마도 긴긴 문서에 정신을 못차렸을것이다. (필자도 마찬가지이니 기죽지 말자.)
\v{mode-line-format}은 우리가 무엇을 나타내고자 하는지, 리스트 형태로 정의해
주기만 하면된다. 문서에서는 리스트안의 원소가 어떤 타입이냐에 따라 어떻게
해석할지 타입에 따라 하나하나 나열해 놓은 것이다. (쉽게 상상하건데) 만약 Lisp의
심볼이 원소이면 심볼을 해석(eval)해서 상태바에 포함시킬 것이고, `:eval`의
심볼(단순한 ":eval" 이름의 심볼)로 시작하는 리스트라면 `:eval`을 제외한 나머지
원소들을 해석해서 출력할 것이다. 물론 편의를 위해 자주 쓰이는 기능들은
"%b"(버퍼이름)와 같은 문자열로 정의되어 있으니, 아래에 정의된 문자열을 설명해
놓았다. 

간단하게 한번 실험해 볼까? 버퍼이름을 나타내는 정의된 문자열 "%b"와 현재 버퍼의
파일이름을 담고 있는 \v{buffer-file-name}을 리스트에 넣어 정의해
보자. ("enc.c" 버퍼에서 \k{M-:} 실행 후 표현식을 입력한다.)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(setq mode-line-format (list "%b" " => " 'buffer-file-name))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

상태바가 아래와 같이 변경되었는가?

    -----------------------------------------------------------------------
    enc.c => /tmp/enc.c
    =======================================================================

다음은 복잡하게 정의된 (하지만 일반적으로 쓰이는) 하나의 원소인데, 우리가
마우스를 상태바에 가져가면 나타나는 툴팁(help-echo)과, 색깔(face)를 같이 정의
하고 있다. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(setq mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name))))
   ....
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

이맥스가 어떻게 글자를 출력하고, 색깔을 입히는지 궁금해졌는가? 이맥스에서 글자들을
어떻게 정형화 해서 특성을 정의하고, 출력하는지 다음장에서 차근차근 알아볼 것다.

# 버퍼 로컬 변수

"enc.c" 버퍼의 모드라인이 변경되었는가? 그렇다면 다른 버퍼의 상태바는 어떠한가?
\*Scratch\* 버퍼로 이동해보자. 이전 상태바 모습 그대로 출력되고 있는가, 아니면
우리가 변경한 상태바 문자열로 변경되었는가? 신기하게도 우리가 수정한
버퍼이외에는 모두 이전 상태바를 여전히 가지고 있다.

자 \v{mode-line-format} 변수에 관한 문서를 다시 살펴 보자.

    mode-line-format is a variable defined in `C source code'.
    Its value is shown below.
    
      Automatically becomes buffer-local when set in any fashion.
      This variable is potentially risky when used as a file local variable.
    ...

만약에 어떠한 방법으로든 \v{mode-line-format}의 값을 변경하면 "buffer-local"이
된다고 한다! 일반적인 프로그래밍 언어에서 전역 변수, 지역 변수의 개념이
있다. 이맥스 Lisp에는 하나의 재미있는 변수의 범위가 하나더 있는데 이것이 "버퍼
로컬"이다. \v{mode-line-format}의 값이 버퍼마다 다르듯, 버퍼마다 독립적으로
생성되는 환경이라고 생각할 수 있다.

# 환경(environment)과 동적 바인딩(dynamic binding)

이전 장에서 설명했듯이 환경은 심볼(키)과 이에 해당하는 값이 기록되는 메모리 같은
공간이다. 다른 언어에서 map, directory, hashtable 이라는 구조체로 표현 될 수
있겠다. 

    +------------------+ (*)
    | local (func) env | ...
    +---------|--------+
    +---------V--------+ (*)
    | local (func) env | ...
    +---------|--------+
    +---------v-------------+ (+)
    |    buffer-local env   |  ....
    +---------|-------------+
    +---------v---------------------------------------+ (1)
    |                    global env                   |
    +-------------------------------------------------+

Lisp에서 변수의 범위(scope)는 동적 환경(environment)에서 

% auto-mode-alist

