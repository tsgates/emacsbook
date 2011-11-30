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

![\n{img} 테트리스 실행하기](\s{snap -s 80x25 -o tetris-score.png -c 
 M-x "\"tetris\"" RET M-: "\"(set 'tetris-score 100)\"" RET SPACE w3
 M-: "\"(set 'tetris-score 100)\""})

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
