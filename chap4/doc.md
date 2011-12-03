%
% Taesoo Kim
%
% title: Lisp 이해하기2
%
% abstract: Lisp의 특수 형태/연산자들인 조건문, 변수 선언, 함수 선언의 개념을
% abstract:  이맥스의 테트리스 게임의 소스 코드를 통하여 이해해본다.
%

3장에서 Lisp의 기본적인 특성/개념들을 살펴보았다. 이번 장에서는 Lisp의
특수 형태/연산자에 대해서 살펴보고, 기본적인 프로그래밍에 필요한 조건문, 반복문
등을 테트리스 게임에서 어떻게 쓰고 있는지 이해해본다. 이번 장을 끝으로 Lisp에
대한 기본적인 설명은 마치고, 본격적으로 이맥스에 대해 알아볼 테니 지겹더라도
조금만 참고 따라오기 바란다.

# 환경 (Environment)

Lisp에서 심벌(symbol)을 계산(evaluate)하면 정수(integer), 문자열(string),
심벌(symbol), 그리고 리스트(list)의 값(value)이 된다. 이렇게 값을 가지고 있는
심벌들을 변수(variable)라고 부른다. 만약 값이 없는 심벌을 계산하면 어떻게 될까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
ELISP> no-such-a-var
*** Eval error ***  Symbol's value as variable is void: no-such-a-var
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

주어진 심벌의 변수로서의 값이 없다는 에러를 볼 수 있다. 특히 이러한 변수 및
함수가 저장되는 공간을 환경(environment)이라고 부른다. 위의 에러는 주어진 환경에
심벌에 해당하는 값을 찾을 수 없음을 의미한다. 기본적으로 환경은 이름(심벌) ->
값의 매핑을 담고 있는 단순한 저장 공간이다.

테트리스를 \k{M-x tetris}를 실행해보자.

![\n{img} 테트리스 실행하기](\s{snap -s 80x25 -o tetris-screen.png -c 
 M-x "\"tetris\"" RET M-x "\"tetris\"" w3})

테트리스 프로그램에서는 무엇을 변수로 정의하고 있을까? 많은 변수들이 있겠지만,
오른쪽 화면에 보이는 점수가 변수로 정의되어 사용되고 있음을 쉽게 추측해 볼 수
있다. 이맥스 테트리스에서는 \v{tetris-score}가 변수로 사용되고 있고, 이는 \k{C-h
v: 변수 도움말} (__h__elp __v__ariable)을 통해서 어떻게 쓰이고, 어떠한 값을 갖고
있는지 쉽게 확인해 볼 수 있다.

    tetris-score is a variable defined in `tetris.el'.
    Its value is 0
    Local in buffer *Tetris*; global value is 0
    
      Automatically becomes buffer-local when set in any fashion.
    
    Documentation:
    Not documented as a variable.

위의 도움말은 프로그래머들에게는 상식선에서 모두 이해가 될 것이다. 정리하면
\v{tetris-score}의 값은 0이고, \*Tetris\* 버퍼에 지역 변수(local variable)며,
전역 변수(global variable)의 값은 0이다. 또한 만약 값을 할당하면 자동적으로
버퍼의 지역변수가 된다. 할당? 지역변수? ...

먼저 어떻게 심벌에 값을 할당할 수 있을지 알아볼까?

# 특별 형태 (Special Forms)

많은 프로그래머들이 어린 시절 게임을 통해서 컴퓨터를 접했을 것이다. 필자도 예외는
아니었는데, 게임에 소질이 없어 항상 게임 잘하는 친구들을 부러워하곤 했다. 하지만
필자는 게임핵?이라는 메모리 수정/해킹하는 툴로 항상 친구들 보다 높은 점수를 얻곤
했었는데, 이맥스 테트리스에서도 가능할까?

먼저 테트리스 \f{M-x tetris}를 실행하자. 3장에서 배운 표현식을 계산하는 방법 중
유일하게 현재 버퍼를 떠나지 않고 표현식을 계산하는 방법이 있었는데 기억하는가?
(너무 큰 기대인가?) \k{M-:}를 입력하고, 다음과 같이 입력해 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
Eval: (setq tetris-score 100)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 표현식은 \f{setq: 심벌에 값을 할당} 함수를 이용해 \v{tetris-score}의 심벌에
100이라는 정수 값을 할당한다. 재미있는 사실은, 3장에서 배운 여러 표현식 계산하는
방법 중 \f{M-:}를 이용하는 방법은 유일하게 현재 버퍼의 환경(지역변수들을 담고
있는)에서 표현식을 계산한다는 것이다. 즉 위와 같이 입력하여 \*Tetris\* 버퍼의
지역 변수 \v{tetris-score}에 값을 100으로 수정 할 수 있었다. 값을 수정하였는데도
점수가 화면에 곧바로 반영되지 않는데, 테트리스는 정상적으로 점수를 획득하는
순간마다 화면에 업데이트하기 때문이다. 현재 블록을 밑으로 내려 볼까?

![\n{img} 테트리스 점수 수정하기](\s{snap -s 80x25 -o tetris-score.png -c 
 M-x "\"tetris\"" RET M-: "\"(setq tetris-score 100)\"" RET SPACE w3
 M-: "\"(setq tetris-score 100)\""})

위의 리스트 형태의 표현식을 Lisp의 계산식 방법에 따라 계산해 보자.

1. \f{setq}의 심벌에 해당하는 함수를 찾고,
1. \v{tetris-score}의 심벌에 해당하는 값 0과,
1. 100(아톰)은 그대로의 값을 가지므로, 값 100을 인자로
1. \f{setq}의 함수를 호출한다.

\f{setq} 함수는 정수 0과 100을 인자로 받았는데, 어떻게 \v{tetris-score}의 값을
변경할 수 있었던 것일까? 실제로 두 번째 과정에서 \v{tetris-score}이 값으로
계산되지 않고 심벌이 전달된다. 도움말을 살펴보자.

    setq is a special form in `eval.c'.
    
    (setq [SYM VAL]...)
    
    Set each SYM to the value of its VAL.
    The symbols SYM are variables; they are literal (not evaluated).
    The values VAL are expressions; they are evaluated.
    Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
    ...

우리는 Lisp의 일반적인 계산법을 따르지 않는 연산자를 특수 연산자 (special
operator) 또는 특수 형태 (special form)이라고 부른다. Lisp에 이러한 특수
연산자들에는 심벌에 값을 할당, 함수를 선언, 조건문, 매크로가 있다.

심벌을 생성하는 것, 함수를 선언하는 것은 기본적인 계산 규칙으로 할 수 없으므로
특수 형태를 띄어야 할 것 같은데 왜 조건문은 특수 형태를 띄어야 할까?

# 조건문 (Condition)

사실 모든 함수형 (pure functional language)에서 대부분의 조건문은 특수한 형태를
갖는다. Lisp도 예외는 아닌데, 먼저 어떻게 사용하는지 알아보자. 그럼 테트리스의
어떤 코드가 조건문을 사용할까? 쉽게 추측건대 종료하는 조건을 확인하는 함수가
있을 것이다.

![\n{img} 테트리스의 종료 조건](\s{snap -s 80x25 -o tetris-end.png -c
 M-x "\"tetris\"" RET SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE})

테트리스가 작동하는 원리는 생각해보자. 테트리스에서 일단 블록이 하나 만들어지면,
블록이 주기적으로 화면에서 한 칸씩 내려오게 되다. 이 주기에 맞추어 사용자가 입력을
하면 입력에 따라 좌/우로 블록을 움직이게 되고, 블록이 화면의 끝이나 다른 블록에
닿은 경우 새로운 블록이 생성된다. 만약 화면에 새로운 블록이 생성될 곳이 없으면
게임이 종료된다. 아하, 새로운 블록이 생성되는 함수?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
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
    (tetris-update-score)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위에서부터 한 줄 한 줄 살펴보자. 첫 줄에서 \f{tetris-new-shape} 함수를 선언
\f{defun: 함수 정의}(__def__ine __fun__ction)하고, 두 번째 줄부터 함수의 정의가
시작된다.

자자. 1분 동안 위의 코드를 가만히 읽어보자.

이제 아래 설명과 같이 읽어 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
(defun tetris-new-shape ()
  ;; 다음 블록(오른쪽 상단의)을 현재 블록으로 지정
  (setq tetris-shape tetris-next-shape)
  ;; 초기 블록 방향
  (setq tetris-rot 0)
  ;; 랜덤으로 다음 블록을 지정
  (setq tetris-next-shape (random 7))
  ;; 블록을 x축 중심에 놓음
  (setq tetris-pos-x (/ (- tetris-width (tetris-shape-width)) 2))
  ;; 블록을 y축 시작에 놓음
  (setq tetris-pos-y 0)
  ;; 만약 블록이 겹치면,
  (if (tetris-test-shape)
      ;; 게임을 종료
      (tetris-end-game)
    ;; 아니면, 블록을 화면에 출력하고
    (tetris-draw-shape)
    ;; 다음 블록도 (오른쪽 상단에) 출력하고
    (tetris-draw-next-shape)
    ;; 점수를 반영한다.
    (tetris-update-score)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

관행적으로 Lisp 모듈에서 사용되는 함수/변수 앞은 모듈의 이름으로 시작한다. 즉
테트리스 모듈에서는 tetris를 앞 글자로 사용한다. 재미있는 실험을 하자. 변수의
이름에 있는 tetris는 tetris's로 함수에 있는 tetris는 제거해볼까?

    defun new-shape
      set tetris's shape (with) tetris's next-shape
      set tetris's rot (with) 0
      set tetris's next-shape (with) random 7
      set tetris's pos-x (with) (/ (- tetris's width (tetris's shape-width)) 2)
      set tetris's pos-y (with) 0
      if test-shape
           end-game
        draw-shape
        draw-next-shape
        update-score

조금 코드를 보기 수월한가? 함수(리스트의 첫 원소)는 모두 동사(명령)로
시작하고, 변수들은 모두 명사로 시작하는 것을 알아차렸는가? Lisp은 일반적인
언어에서 infix로 사용하는 연산자를 prefix로 사용한다. 그 결과 신기하게도 동사인
함수(연산자)와 명사의 인자들이 영어의 문법에 맞추어 읽혀지는 현상을 볼 수 있다.

이번 절의 목적인 조건문(if statement)에 대해 다시 살펴보면,

    if is a special form in `eval.c'.
    
    (if COND THEN ELSE...)
    
    If COND yields non-nil, do THEN, else do ELSE...
    Returns the value of THEN or the value of the last of the ELSE's.
    THEN must be one expression, but ELSE... can be zero or more expressions.
    If COND yields nil, and there are no ELSE's, the value is nil.

Lisp의 조건문은 특성은 아래와 같이 정리 할 수 있다. 

1. 특별 형태 (special form)
1. 조건문 `COND`이 \v{nil}이 아니면 `THEN` 표현식을 계산 후 결과를 리턴
1. 그렇지 않으면 나머지 모든 표현식 `ELSE...`를 계산 후 마지막 표현식의 결과를 리턴

조건문은 왜 특수 형태일까? 만약 특수 형태가 아닌 리스트 형태의 표현식이었다면
계산하기 위해 함수 인자 위치에 있는 `THEN`과 `ELSE...`의 모든 표현식들을 계산해야
한다. 즉 조건문은 `COND`의 표현식에 결과에 따라 선택적으로 `THEN`과
`ELSE...`표현식을 계산해야 하므로, 조건문은 특수 형태를 갖게 되었다.

하나 더 강조하자면 조건문의 표현식도 계산 후 결과 값을 갖는다. 다음의 예를 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
ELISP> (message "hello %s" (if (= 1 2) "hello" "world"))
"hello world"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 조건문의 조건이 \v{nil}이므로 후자의 표현식을 계산 후 결과값인 "world"
문자열을 조건문의 결과값으로 리턴하였다.

# 함수 정의 (defun)

환경의 이름(심벌)을 생성하는 방법은 크게 두 가지, 변수를 선언하거나, 함수를 선언
하는 방법이 있다. 우리가 이미 살펴본 방법은 변수 중 전역 변수(심벌과 해당 값)를
생성 하는 방법으로 특수 형태의 함수, \f{setq}를 이용하는 방법이었다. 이번 절에서는
변수 중 지역 변수(새로운 환경 안에서의 심벌과 해당 값)를 생성하는 함수(연산자)와
함수를 선언하는 특수 형태의 함수(연산자)를 알아보겠다.

두 가지의 함수 형태를 볼 것인데, 하나는 테트리스를 시작하는 함수
`tetris-start-game`이고 나머지 하나는 주기적으로 게임을 진행하는 함수
`tetris-update-game` 함수이다. 잘 따라오면서 코드를 읽기 바란다. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
;; defun = define function, 함수를 정의
;;  - tetris-start-game의 심벌에 해당 값으로 함수 정의를 바인딩한다.
;;  - `()`는 `nil`과 같은 심벌로 인자를 받지 않음을 뜻한다.
(defun tetris-start-game ()
  ;; defun 함수의 첫 번째 표현식(리스트의 첫 번째 인자)은 해당 함수의 문서로 문자열
  "Starts a new game of Tetris"
  ;; 해당 함수를 사용자에게 노출 (2장 참고 - 상호작용하는 함수) 
  (interactive)
  ;; 게임을 리셋
  (tetris-reset-game)
  ;; map: 키와 해당 함수를 담고 있는 변수
  ;;  (예)
  ;;  - "n" -> 'tetris-start-game
  ;;  - "q" -> 'tetris-end-game
  ;; 현재 키에 테트리스 게임용 함수를 바인딩
  (use-local-map tetris-mode-map)
  ;; let: period의 심벌에 오른쪽 값을 바인딩 (밑에서 살펴본다)
  ;; 테트리스가 업데이트되는 주기를 결정
  (let ((period (or (tetris-get-tick-period)
                    tetris-default-tick-period)))
    ;; 주기 (period)마다 tetris-update-game 함수를 호출하는 타이머 시작
    (gamegrid-start-timer period 'tetris-update-game)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

복잡한가? 코드를 보는 관점을 바꾸어 보자. \f{tetris-start-game}의 함수는 어떠한
일을 할까? 함수 정의를 읽지 않고는 함수의 정확한 일을 알 수가 없다. 하지만 우리가
추측 할 수 있는 방법들이 있는데 첫째는 함수의 이름을 통해서고, 둘째는 주석을
통해서다. 즉 \f{tetris-start-game}의 이름을 통해서 우리는 새로운 게임을 시작하는
함수라는 추측을 할 수 있다. 그 내부는 어떠한가? \f{let}을 제외하고는 단순 함수
호출의 나열이다. 같은 맥락으로 호출하는 함수 내부를 들여다 보지 않고 이름을 통한
추측(abstraction)만으로도 정의하고 있는 함수를 이해하는데 문제가 없다.

그러면 \f{let}은 왜 사용하는가? 코드를 명확하게 하는 또 한 가지 방법은 값들에
프로그래머가 읽을 수 있는 이름을 할당하는 것인데 \f{let} 구문 없이 아래와 같이
코드가 짜였다고 생각해 보자. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
  (gamegrid-start-timer (or (tetris-get-tick-period)
                            tetris-default-tick-period)
                        'tetris-update-game)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}

위의 코드를 글을 읽듯이 읽어보면, 문장이 길어지고 하고자 하는 일이 무엇인지
한눈에 파악하기 힘들다. 프로그래머가 \f{let}을 사용하면, `gamegrid-start-timer`의
첫 인자에 들어가는 값은 '주기'이다라고 명시적으로 말을 할 수 있게 된다. 위의
함수가 주석 하나 없이 짜였음에도 불구하고 코드를 읽는 사람에게 정확한 목적을
전달할 수 있는 것은 명확한 함수와 변수의 심벌(이름)을 사용하고 있기 때문이다.

스스로 다음 코드를 읽어보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
(defun tetris-update-game (tetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not tetris-paused)
           (eq (current-buffer) tetris-buffer))
      (let (hit)
        (tetris-erase-shape)
        (setq tetris-pos-y (1+ tetris-pos-y))
        (setq hit (tetris-test-shape))
        (if hit
            (setq tetris-pos-y (1- tetris-pos-y)))
        (tetris-draw-shape)
        (if hit
            (tetris-shape-done)))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

잘 읽히는가? 그러면 `tetris-shape-done` 함수는 무슨 일을 할까? 만약 우리가 처음
살펴본 `tetris-new-shape`를 호출할 것이라는 추측을 했다면! 이번 장도
대성공이다. 위의 함수는 매 주기마다 호출되어 현재 블록을 아래로 한 칸씩 내리고
만약에 화면이나 다른 블록에 닿았다면 블록을 멈추고 새로운 블록을 생성하는
코드이다.

마지막으로 강조하고 싶은 `tetris-update-game` 함수와 `tetris-start-game` 함수의
다른 점 두 가지는 아래와 같다

1. `tetris-buffer`를 인자로 받고, 
1. \f{interactive}의 호출이 없다. 

즉, `tetris-start-game` 함수는 사용자에게 노출되어 키보드의 'n'키를 입력하거나
\k{M-x tetris-start-game}을 통해 함수를 호출할 수 있는데 반하여,
`tetris-update-game`은 사용자가 직접 호출할 수 없고 타이머에 의한 간접적인
호출만 가능하다는 것이다.

정리하면, 함수를 정의하는 방법은? 문서를 찾아보는 게 빠르지 않을까?

    defun is a special form in `eval.c'.
    
    (defun NAME ARGLIST [DOCSTRING] BODY...)
    
    Define NAME as a function.
    The definition is (lambda ARGLIST [DOCSTRING] BODY...).
    See also the function `interactive'.

함수는 \f{defun}의 특수 형태를 이용하여 정의하고, 첫 인자 `NAME`은 함수 이름,
두 번째 인자 `ARGLIST`는 정의할 함수에 사용될 인자의 리스트, 세 번째 인자
`[DOCSTRING]`은 해당 함수의 문서로 생략 가능하며, 나머지 인자들 `BODY...`은 함수의
정의를 의미한다. 다른 특수 형태의 함수들과 같은 맥락으로 함수를 정의하기 위해
각각의 인자와 함수 정의를 계산하지 않아야 하므로 \f{defun}함수 또한 특수 형태를
띄고 있다.

문서에 있는 `interactive` 링크를 따라가고 싶은가? (다음 장에서 알아본다!)

# 정리

이번 장에서는 다음과 같은 Lisp의 특수 형태들에 대해서 알아보았다.

- \f{if} : 조건문
- \f{defun} : 함수 심벌 -> 함수 정의
- \f{setq} : 전역 변수 심벌 -> 값
- \f{let} : 지역 변수 심벌 -> 값

다음 장에서는 이맥스 관점에서 Lisp이 어떠한 DSL(Domain Specific Language)을
제공하는지, 어떠한 특징들이 이맥스를 확장 가능하게 하는 건지, 그리고 Lisp 함수를
처음으로 구현해 보도록 할 것이다. 코딩 타임!