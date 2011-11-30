%
% Taesoo Kim
%
% title: Lisp 이해하기2
%
% abstract: Lisp의 특수형태/연산자들을 이해하고, 이멕스의 테트리스 게임의
% abstract: 소스코드를 같이 읽어본다.
%

3장에서 Lisp의 기본적인 개념들을 살펴보았다. 이번 장에서는 Lisp의
특수형태/연산자에 대해서 이해해본 후, 기본적인 프로그래밍에 필요한 조건문, 반복문
등을 테트리스 게임에서 어떻게 쓰고 있는지 이해해본다. 이번 장을 끝으로 Lisp에
대한 기본적인 설명은 마치고, 본격적으로 이맥스에 대해 알아볼테니 지겹더라도
조금만 참고 따라오기 바란다.

# 환경 (Environment)

Lisp에서 심볼(symbol)을 계산(evaluate)하면 정수(integer), 문자열(string),
심볼(symbol), 그리고 리스트(list)의 값(value)이 된다. 이렇게 값을 가지고있는
심볼들을 변수(variable)라고 부른다. 만약 값이 없는 심볼을 계산하면 어떻게 될까?

    ELISP> no-such-a-var
    *** Eval error ***  Symbol's value as variable is void: no-such-a-var

즉, 주어진 심볼의 변수로서의 값이 없다는 에러를 볼 수 있다. 특히 이러한 변수 및
함수가 저장되는 공간을 환경(environment)라고 부른다. 위의 에러를 다시 해석하면,
주어진 환경에 심볼에 해당하는 값을 찾을 수 없다고 이해할 수 있겠다. 간단하게
말해서 환경은 이름 -> 값의 단순한 형태의 저장소이다.

테트리스를 \k{M-x tetris}를 실행해보자.

![\n{img} 테트리스 실행하기](\s{snap -s 80x25 -o tetris-screen.png -c 
 M-x "\"tetris\"" RET w3})

테트리스 프로그램에서는 무엇을 변수로 정의하고 있을까? 많은 변수들이 있겠지만,
오른쪽 화면에 보이는 점수가 쉽게 변수로 정의되어 사용되고 있음을 추측해 볼 수
있다. 테트리스에서는 \v{tetris-score}가 변수로 사용되고 있고, 이는 \k{C-h v: 변수
도움말} (__h__elp __v__ariable)을 통해서 어떻게 쓰이고, 어떠한 값을 갖고 있는지
쉽게 알아 볼 수 있다.

    tetris-score is a variable defined in `tetris.el'.
    Its value is 0
    Local in buffer *Tetris*; global value is 0
    
      Automatically becomes buffer-local when set in any fashion.
    
    Documentation:
    Not documented as a variable.

위의 도움말의 용어들은 프로그래머들에게 친숙해 상식선에서 모두 이해가 된다.
\v{tetris-score}의 값은 0이고, \*Tetris\* 버퍼에 지역 변수(local variable)며,
전역 변수(global variable)의 값은 0이다. 만약 값을 할당하면 자동적으로 버퍼의
지역 변수가 된다.

그러면 어떻게 심볼에 값을 할당할 수 있을까?

# 특별 형태 (Special Forms)

많은 프로그래머들이 어린 시절 게임을 통해서 컴퓨터를 접했을 것이다. 필자도 예외는
아니였는데, 게임에 소질이 없어 항상 게임잘하는 친구들을 부러워 하곤 했다. 하지만
필자는 게임핵?이라는 메모리 수정/헤킹하는 툴로 항상 친구들 보다 높은 점수를 얻곤
했었는데, 이맥스 테트리스에서도 한번 해볼까?

먼저 테트리스 \f{M-x tetris}를 실행하자. 3장에서 배운 표현식을 계산하는 방법 중
유일하게 현재 버퍼를 떠나지 않고 표현식을 계산하는 방법이 있었는데 기억하는가?
\k{M-:}를 입력하고, 다음과 같이 입력해 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
Eval: (setq tetris-score 100)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 표현식은 \f{setq: 심볼에 값을 할당} 함수를 이용해 \v{tetris-score}의 심볼에
100이라는 정수 값을 할당한다. 재미있는 사실은, 3장에서 배운 여러 표현식 계산하는
방법중 \f{M-:}를 이용하는 방법은 유일하게 현재 버퍼의 환경(지역변수들을 담고
있는)에서 표현식을 계산한다는 것이다. 즉 위와 같이 입력하여 \*Tetris\* 버퍼의
지역 변수 \v{tetris-score}에 값을 100으로 수정 할 수 있었다. 값을 수정하였는데도
점수가 화면에 곧바로 반영되지 않는데, 테트리스는 정상적으로 점수를 획득하는
순간마다 화면에 업데이트하기 때문이다. 현재 블락을 밑으로 내려 볼까?

![\n{img} 테트리스 점수 수정하기](\s{snap -s 80x25 -o tetris-score.png -c 
 M-x "\"tetris\"" RET M-: "\"(setq tetris-score 100)\"" RET SPACE w3
 M-: "\"(setq tetris-score 100)\""})

위의 리스트 형태의 표현식을 Lisp의 계산식 방법에 따라 계산해 보자.

1. \f{setq}의 심볼에 해당하는 함수를 찾고,
1. \v{tetris-score}의 심볼에 해당하는 값 0과,
1. 100(아톰)은 그대로의 값을 갖으므로, 값 100을 인자로
1. \f{setq}의 함수를 호출 한다.

\f{setq} 함수는 정수 0과 100을 인자로 받았는데, 어떻게 \v{tetris-score}의 값을
변경할 수 있었던 것일까? 실재로 두번째 과정에서 \v{tetris-score}이 값으로
계산되지 않고 심볼이 전달된다. 도움말을 살펴 보자.

    setq is a special form in `eval.c'.
    
    (setq [SYM VAL]...)
    
    Set each SYM to the value of its VAL.
    The symbols SYM are variables; they are literal (not evaluated).
    The values VAL are expressions; they are evaluated.
    Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
    ...

우리는 Lisp의 일반적인 계산법을 따르지 않는 연산자를 특수 연산자 (special
operator) 또는 특수 형태 (special form)이라고 부른다. Lisp에 이러한 특수
연산자들에는 심볼에 값을 할당, 함수를 선언, 조건문, 메크로가 있다.

심볼을 생성하는것, 함수를 선언하는것은 기본적인 계산 규칙으로 할 수 없으므로
특수형태를 띄어야 할 것 같은데 왜 조건문은 특수형태를 띄어야 할까?

# 조건문 (Condition)

사실 모든 함수형 (pure functional language)에서 대부분의 조건문은 특수한 형태를
갖는다. Lisp도 예외는 아닌데, 먼저 어떻게 사용하는지 알아보자.

![\n{img} 테트리스의 종료조건](\s{snap -s 80x25 -o tetris-end.png -c 
 M-x "\"tetris\"" RET SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE}
 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scheme}
(defun tetris-draw-shape ()
  (loop for y from 0 to (1- (tetris-shape-height)) do
	(loop for x from 0 to (1- (tetris-shape-width)) do
	      (let ((c (tetris-get-shape-cell x y)))
		(if (/= c tetris-blank)
		    (gamegrid-set-cell (+ tetris-top-left-x
					  tetris-pos-x
					  x)
				       (+ tetris-top-left-y
					  tetris-pos-y
					  y)
				       c))))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 함수 (function)

 - defun (define function)

# 조건문 (control)

# 제어문 (loop)

# 특별 형태 (special forms)

- evaluate
 - eval & apply
- control
 - if/else
- loop
 - while/for alternatives
- scope
 - let

# emacs

- universal argument C-u
