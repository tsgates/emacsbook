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

![\n{img} 테트리스 실행하기](\s{snap -s 80x10 -o tetris-screen.png -c 
 M-x "\"tetris\"" RET w3})

이는 no-such-a-var라는 심볼에 바인드된 값(void-variable)이 없음을 의미한다.
그러면 다음과 같은 표현식은 어떻게 해석될까?

    (message fill-column)

아래와 같은 에러가 발생하고,

    Debugger entered--Lisp error: (wrong-type-argument stringp 70)
      message(70)
      eval((message fill-column))
      ...

이전과 다른점은 message와 fill-column 각각의 심볼에 바인드된 함수와 값을 찾았고
message 함수를 70의 인자를 가지고 evaluate하는 과정에서 에러가 발생함을 알 수
있다. "(message 70)"이 아니라 "message(70)"임을 확인해보자. 위의 에러는 message의
첫 인자는 문자열이어야 하는데, 우리는 string이 아닌 70을 가지고 호출했기 때문에
발생했다.

자 그럼 message는 c 언어의 rvalue처럼 함수정의를 나타낸다는 것을 이해했을
것이다. 그러면 message라는 심볼을 어떻게 표현 할까? Lisp에서는 ' 를 앞에 붙여
'message라고 쓰면 message라는 이름의 심볼을 의미한다.


앞으로 설명하겠지만 한가지 차이점은 \k{M-:}을 이용한 해석은 현재 사용자에게
보이는 버퍼의 환경에서 해석이되는 것이고, \*scratch\* 버퍼에서 해석한 경우
\*scratch\* 버퍼의 환경에서 표현식을 해석하는 것이다.

# 특별 형태 (special forms)

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