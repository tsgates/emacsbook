%
% Taesoo Kim
%
% title: 키맵(Keymap)과 커서이동
%
% abstract: 이맥스에서 어떻게 키입력을 처리하며 어떠한 Lisp의 구조를 사용하는지
% abstract: 이해하고, 전역적으로 사용되는 키입력과 함수 중 특별히 커서의 이동과
% abstract: 관련된 함수들을 살펴본다.
%

이번 장에서는 이맥스에서 사용자의 키입력을 어떻게 처리하는지, 또 어떠한 Lisp의
데이터구조로 이를 표현하는지 이해해보고, 더불어 이맥스의 가장 기본적인 편집기능
인 커서이동에 관한 함수들을 하나하나 살펴보도록 한다.

# 키맵 (Keymap)

사용자가 키를 입력하면 이맥스는 키입력에 바인드된 함수를 찾아 호출한다. 키입력과
바인드된 해당 함수의 정보를 담고있는 변수를 이맥스에서 맵(map)이라고 부른다. 맵은
아래와 같이 키와 값을 짝으로 갖는 Associate List(Alist)의 구조로
표현된다. Alist는 리스트의 특별한 형태로, 키와 해당 값이 리스트의 하나의 원소로
저장된다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
;; global-map
(list ("C-a" . 'move-beginning-of-line)
      ("C-e" . 'move-end-of-line)
      ...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

즉 맵의 한원소는 "C-a"와 같은 입력키, \f{move-beginning-of-line}와 같은 해당
함수의 심볼로 이루어진다. 이맥스는 여러 개의 맵을 가지고 있고, 이들을 체인으로 엮어
관리한다. 사용자가 키를 입력하면 위의 맵부터 아래로, 순차적으로 해당 함수를 찾게
되며 가장 먼저 찾은 키의 해당 함수를 호출한다. 체인으로 엮인 구조를 살펴보면
아래와 같다.

             +---------- 사용자의 키 입력
             |
    +--------v--------+
    | local-map?      | (("C-a" . `soo-magic-beg-of-line) ...)
    +--------|--------+
    +--------v--------+
    | minor-mode-map* | (("C-." . `flyspell-auto-correct-word) ...)
    +--------|--------+
    +--------v--------+
    | major-mode-map+ | (("M-e" . `c-end-of-statement) ...)
    +--------|--------+
    +--------v--------+
    | global-map      | (("C-a" . `move-beginning-of-line) ...)
    +-----------------+

위쪽 부터 3개의 맵들, \v{local-map}, \v{minor-mode-map}, \v{major-mode-map}은
현재 버퍼 안에서의 키입력에 영향을 미치는 맵이고, \v{global-map}은 전역적인
이맥스의 키입력에 영향을 미치는 맵이다.

이맥스에서 우리의 키입력이 현재 버퍼에서 어떤 맵에 의해서 어떠한 함수로
해석되는지 알아 볼 수 있는데, \k{C-h b:바인딩 도움말} (**h**elp **b**inding)을
입력해보자. 입력한 키에 대해서 위에서부터 차례로 해당하는 함수를 찾게되며 그 중
우리가 특별하게 관심있는 \v{global-map}을 찾아 보면, 아래와 같은 화면을 볼 수
있다.

![\n{img} \v{global map}](\s{snap -o emacs-help-binding.png
  -c C-h b C-x 0 C-s "\"global\""})

\k{C-h k}의 입력된 키에 대한 함수를 찾는 것과는 달리, 지금 알아본 \k{C-h b}는
사용자가 현재 버퍼에 대하여 입력할 수 있는 모든 키 입력과 해당하는 함수를
한눈에 볼 수 있다. 즉 사용자가 현재 수정하고 있는 버퍼에 어떠한 특별한 기능들이
제공되고 있으며, 어떠한 함수들이 바인딩 되어 있는지 하나하나 살펴볼 수 있다.

# 모드 (Mode)

일반적으로 우리가 특정 파일을 이맥스에서 열었을 때 이맥스는 열린 파일에 대한
버퍼를 생성하고, "버퍼"라는 이름에서 알 수 있듯이 사용자는 파일을 직접 수정하는
것 대신 버퍼를 수정하게 된다. 이때 현재 작업하는 버퍼(파일)에 대하여 특별히
유용한 함수들과 키맵(map)의 그룹을 불러 들이게 된다. 예를 들면 C언어로 짜여진
파일을 열었을 경우, C언어에 특화된 함수와 변수(코드 하이라이팅, 주석 처리 등)를
불러들이고, 이러한 함수와 변수를 하나로 모아서 모드(mode)라고 부른다. 즉 C언어로
짜여진 파일을 열면 (".c"나 ".h"의 확장자를 갖는 파일을 열면), 현재 버퍼의
`major-mode`가 `c-mode`가 된다. 버퍼를 기능별로 분류하다 보니, 자연스럽게 하나의
버퍼당 하나의 `major-mode`를 갖게 되며, 현재 버퍼의 모드는 `major-mode`의 변수에
기록이 된다. \f{C-h v: 변수 도움말}을 통해 현재 버퍼의 주(major) 모드가 무엇인지
확인해 보도록 하자.

버퍼에 하나씩 할당되는 기본적인 주 모드 이외에, 유틸리티적 특성을 갖는 함수와
변수의 집합을 하나로 묶어 `minor-mode`라고 부른다. 예를 들면 스펠링 체크를 위한
`flyspell-mode`와 편리한 네비게이션을 위한 `ido-mode` 등이 있다. 이와 같은
`minor-mode`는 하나의 버퍼에 여러 개의 모드를 적용할 수 있고, `major-mode`와
유사하게 `minor-mode-list`의 변수에 기록된다.

\t{highlight modlines in the picgure}
![\n{img} Major/Minor Modes](\s{snap -o emacs-modes.png -s 80x15
  -c -a /usr/include/stdio.h C-x 1 C-s "\"STDIO\"" 
  C-g M-x "\"flyspell-prog-mode\"" RET})

\t{make sure this status was explained in the first chapter}

위의 화면에서는 아래 상태바에서 볼 수 있듯이 "stdio.h"파일을 열었고, 이에
해당하는 `major-mode`로 `c-mode`를 사용하고, 부가적인 `minor-mode`로
`flyspell-mode`를 사용하고 있다.

정리하면, 하나의 모드(mode)는 유사한 기능을 위한 함수/변수들과, 사용자가 어떠한
키입력으로 호출하는지에 관한 맵(map)으로 이루어져 있다. 하나의 버퍼는 주된 기능을
나타내는 `major-mode` 하나와 사용자에 필요에 따라 여러 개의 `minor-mode`를 불러
들일 수 있고, 사용자가 키를 입력하면 `minor-mode`들과 `major-mode`의 맵을
순차적으로 검색하고 해당 함수를 찾아 호출한다.

`major-mode`와 `minor-mode`는 일반적으로 다른 프로그래머가 일반적인 목적으로
작성한 것으로, 사용자가 버퍼에 해당하는 키입력을 변경하고자 할 때에는,
`local-map`을 변경하여 사용자의 기호에 따라 키바인딩(키와 해당 함수)을 변경하게
된다. 즉, 중복적으로 정의된 키바인딩은 맵을 찾는 순서에 따라, `local-map`에
정의된 키가 가장 우선순위를 갖게 되고, `minor-mode`와 `major-mode`가 차례로
검색된다.

# 커서이동 (Moving)

이맥스의 기본적인 에디팅 기능들(커서이동, 수정, 검색 ..)은 모든 버퍼에 일관적으로
요구되는 기능이기 때문에 \v{global-map}에 정의되어 사용된다. \k{C-h
t:튜토리얼}에서 살펴본 대부분의 에디팅 기능들 역시 \v{global-map}에 정의되어
있으며, 대부분의 경우 특정 모드에 상관 없이 대부분 적용되는 기능들로 이맥스의
일관적인 철학(관행)을 엿볼 수 있기도 하다. 이번 절에서 간단한 커서 이동법을
살펴보도록 하자.

\t{show where the current curosor moves after pressing each key with below codes:
(defun tetris-new-shape ()
  (setq tetris-shape tetris-next-shape)
  (setq tetris-rot 0)
  (setq tetris-next-shape (random 7))
  (setq tetris-pos-x (/ (- tetris-width (tetris-shape-width)) 2))
  (setq tetris-pos-y 0)
  (if (tetris-test-shape)
      (tetris-end-game)
    (tetris-draw-shape)
    (tetris-draw-next-shape)
    (tetris-update-score)))}

첫번째, 가장 작은 단위의 커서 이동은 **글자(char)** 단위의 이동이며, 한글자
앞(**f**orward)으로, 뒤(**b**ackword)로 커서를 이동시키는 \f{forward-char}와
\f{backward-char} 함수들로 `C-`에 바인딩 되어 있다.

    C-f: forward-char
    C-b: backward-char

두번째, **단어(word)** 단위의 커서 이동은 \f{forward-word}와 \f{backward-word}의
함수로 `M-`에 바인딩 되어 있다.

    M-f: forward-word
    M-b: backword-word

세번째, **표현식(s-exp: symbolic expression)** 단위의 커서 이동은 첫번째
두번째 커서이동과 유사하게 \f{forward-sexp}, \f{backward-sexp}의 함수들로
`C-M-`에 바인딩 되어 있다.

    C-M-f: forward-sexp
    C-M-b: bacword-sexp

네번째, **문단(paragraph)** 단위의 커서 이동은 \f{forward-paragraph}과
\f{backward-paragraph}으로 `M-` 키와 `}` (C언어에서 함수의 끝을 나타내는 닫힌
괄호로), `{` (C언어에서 함수의 시작을 나타내는 열린 괄호)로 바인딩 되어 있다.

    M-}: forward-paragraph
    M-{: backward-paragraph

현재 위치에서 앞(**f**orward), 뒤(**b**ackward)로 움직이는 단위는 `C-` (글자) <
`M-` (단어) < `C-M-` (sexp)의 단위로 각각 `f`(앞), `b`(뒤)의 키로 바인딩 되어
있고 특별히 문단단위의 이동은 `M-}`와 `M-{`에 바인딩 되어 있다. s-exp (sexp)의
단위는 다소 생소한 개념인데, Lisp에서 "표현식을 이루는 하나의 단위"라고 생각할 수
있다. 예를 들면 아래와 같다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(setq tetris-next-shape (random 7))
;     o                  : current cursor
;     >o                 : C-f
;     ----->o            : M-f
;     ---------------->o : C-M-f
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

현재 커서의 위치 "t"에서 "tetris"는 하나의 단어(word) 단위고,
"tetris-next-shape"은 sexp의 단위이다. 왜냐하면 현재 위치에서 하나의 표현식을
이루는 최소단위는 "tetris-next-shape"이기 때문이다. ("-"도 함수와 변수의 이름에
허용되는 글자이다.)

이번에는 앞에서 알아본 단위들 보다 조금 큰 범위를 갖는 줄(line), 문장(sentence),
함수(defun), 버퍼(buffer) 단위의 시작(**a**head)과 끝(**e**nd)으로 이동하는
방법들을 알아본다.

첫번째, **줄(line)**단위의 커서 이동은 \f{beginning-of-line}과 \f{end-of-line}
함수들로 `C-`에 바인딩 되어 있다. 이미 `C-b`는 **b**ackward에 바인딩 되어
있으므로, beginning = **a**head로 (또는 첫 알파벳, a) 기억하면 쉽게 기억할 수
있겠다.

	C-a: beginning-of-line (ahead of line)
	C-e: end-of-line

두번째, **문장(sentence)**단위의 커서 이동은 \f{backward-sentence}와
\f{forward-sentence}의 함수들로 `M-`로 바인딩되어 있다.

	M-a: backward-sentence (ahead of sentence)
	M-e: forward-sentence

세번째, **함수(defun)**단위의 커서 이동은, Lisp에서 정의하는 함수 범위의 처음과
끝으로 이동하는 \f{beginning-of-defun}과 \f{end-of-defun}함수들로 `C-M-`에 바인딩
되어 있다.

    C-M-a: beginning-of-defun (ahead of defun)
    C-M-e: end-of-defun

네번째, **버퍼(buffer)**단위의 커서 이동은 `<`(좌로 끝까지 이동하라는 화살표)와
`>`(우로 끝까지 이동하라는 화살표)의 키로 `M-`에 바인딩 되어, 버퍼의 시작과
끝으로 커서를 이동시킨다.
    
    M-<: beginning-of-buffer
    M->: end-of-buffer
 
현재위치에서 처음(**a**head)과 끝(**e**nd)으로 움직이며 `C-` (줄) < `M-`
(문장) < `C-M-` (함수)의 단위가, 각각 "a"(처음)과 "e"(끝)에 바인딩 되어 있고,
특별히 버퍼의 시작과 끝은 `M-<`와 `M->`로 바인딩 되어 있다. 참고로 `C-M-a/e`의
함수(defun) 단위의 움직임은 언어마다 다른 함수 정의와 범위를 가지고 있으므로,
해당 언어의 모드에서 특화된 함수, 예를 들면 C언어의 `c-beginning-of-defun`과 같은
함수로 바인딩 시켜 이맥스의 "관행"을 지켜 사용자가 자연스럽게 사용할 수 있도록
도와준다.

다음으로 알아볼 단위는 다음/이전 줄(line), 화면(page)으로 이동하는 함수들이다.

첫번째, 다음(**n**ext)/이전(**p**revious) **줄(line)**로 이동하는 함수는
\f{next-line}과 \f{previous-line}으로 `C-`에 바인딩 되어 있다.

    C-n: next-line
    C-p: previous-line

두번째, 다음(down)/이전(up)의 **화면(page)**으로 이동하는 함수는 \f{scroll-up}과
\f{scroll-down}으로 각각 `C-`와 `M-`의 대칭의 키에 `v`(아래방향의 화살표) 키로
바인딩 되어 있다.

    C-v: scroll-up
    M-v: scroll-down

이번 절에서 특정 단위들의 앞/뒤, 시작/끝, 다음/이전으로 이동하는 법들을 알아
보았는데, 이 중 가장 처음 알아본 함수, \f{forward-char} 정의를 한번 찾아 볼까?

    C-f runs the command forward-char, which is an interactive built-in function in
    `C source code'.
    
    It is bound to C-f, <right>.
    
    (forward-char &optional N)
    
    Move point right N characters (left if N is negative).

한 글자 앞으로 이동하는 함수 \f{forward-char}는 \k{C-f}와 \k{<right>}(오른쪽
화살표)로 바인딩 되어 있다. 눈에 띄는 점은 함수가 `N`의 인자를
선택적(&optional)으로 받는데, 위의 설명에 따르면 `N`글자만큼 앞으로 이동하며
음수를 받으면 뒤로 이동한다고 한다.

맵(map)의 구조를 살펴보면, 이맥스는 주어진 키에 대한 함수를 호출한다. 그러면
어떻게 \f{forward-char} 함수를 인자를 가지고 호출할 수 있을까?

# 범용 인자 (Universal Argument)

우리가 알아본 커서를 이동시키는 함수들은 하나의 인자를 선택적으로 받는데,
일반적으로 함수에 해당하는 이동단위를 "몇번" 반복하라는 의미를 갖는다. 예를 들면
앞서 알아본 \f{forward-char} 함수는 "몇 글자" 앞으로 가라 \f{next-line}은 "몇
줄" 앞으로 가라라는 인자를 선택적으로 받는다.

\k{C-f}에 바인딩된 \f{forward-char}의 함수에 5(다섯 글자 앞으로 가라)를 인자로
주기 위해서는, \k{C-u 5} 이후 \k{C-f}를 입력하면된다. \k{C-u}를 범용
인자(**u**niversal argument)라고 부른다. 예를 들면 10줄 아래로 이동하기
위해서는 \k{C-u 10} 이후 \k{C-n}를 입력하면된다.

특별히 \k{C-u}이후 아무 숫자도 입력하지 않으면 가장 일반적인 숫자라고 여겨지는
정수 4를 앞으로 호출될 함수의 인자로 넘긴다. \k{C-u}를 두번 입력하면 (4에 다시
4를 곱한) 16을 앞으로 호출될 함수의 인자로 넘기게 된다.

모든 알파벳에 바인딩된 함수 \f{self-insert-command}를 기억하는가? 이 함수 역시
범용 인자를 추가적으로 받는데, \f{C-u} 이후 `a`를 입력하면 알파벳 "a"가 4개
입력되며, \f{C-u 80}이후 `#`를 입력하면 "#"가 80개 입력된다. 이를 통해 단순히
반복되는 입력작업을 피할 수 있다.

# 정리

이번장에서는 어떻게 이맥스에서 키입력을 관리하는지, 또한 전역 키맵(global
keymap)에 정의된 커서 이동하는 법들을 알아 보았다.

글자, 단어, 표현식, 문단의 앞/뒤로 이동하는 함수들:

- `C-f/b`: forward/backward-char
- `M-f/b`: forward/backword-word
- `C-M-f/b`: forward/backword-sexp
- `M-}/{`: forward/backword-paragraph

줄, 문장, 함수, 버퍼의 시작/끝으로 이동하는 함수들:

- `C-a/e`: beginning/end-of-line
- `M-a/e`: backward/forward-sentence
- `C-M-a/e`: beginning/end-of-defun
- `M-</>`: beginning/end-of-buffer

다음/이전 줄, 화면으로 이동하는 함수들:

- `C-n/p`: next/previous-line
- `C/M-v`: scroll-up/down

이들 함수는 범용인자를 받으며, `C-u N`로 N번 반복 할 수 있다.

참고로 위의 관련된 함수와 키는 필자가 임의적으로 기억하기 쉽도록 나눈것으로
이맥스의 개발의도와는 다를 수 있다. 다음 장에서는 이맥스의 시작 과정과 함께
편집, 검색, 윈도우 등의 기능을 알아보도록 할 것이다.
