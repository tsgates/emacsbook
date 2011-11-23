%
% Taesoo Kim
%
% title: Lisp 이해하기
%
% abstract: 이맥스를 확장하고 이해하기 위한 기본적인 Lisp의 개념들을 이해하고,
% abstract: 직접 프로그래밍 해본다.
%

Lisp의 재미있는 특성들을 하나의 장에서 모두 다루기는 불가능하다. 이번장에서는
이맥스를 확장하고 이해하는데 필요한 기본적인 특성들을 알아보자. 먼저 Lisp을
이해하기위한, 궁극적으로는 이맥스를 이해하기위한, 가장 기본이 되는 개념을
살펴보자. 

# 심볼 (Symbol)

아래와 같은 표현식을 다시 살펴보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(message "hello world")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위 표현식은 message라는 함수를 "hello world" 문자열을 인자로 호출하라는
의미이다. 여기서 **message**는 화면 하단에 문자열을 출력하는 함수의
"이름"이다. 즉 message는 함수정의를 *지칭하는* 용도로 사용되고 있다. 뻔한? 소리를
이렇게 강조하는 이유는 Lisp에서는 명시적으로 이름(앞으로 **심볼**)과 함수정의
(또는 변수값)을 분리해서 쓴다. 그렇게 함으로써 언어 특성이 간단해 지고 코드를
데이터처럼? 사용할 수 있게 된다. (앞으로 살펴 볼 것이다!)

Lisp 인터프리터가 위의 표현식을 해석하는 과정을 상상해보자. 먼저 문자열로 주어진
표현식을 받은후, 파싱을 하고, message 심볼에 바인딩(bound)된 함수정의를 찾고
주어진 인자와 함께 evaluate 할것이다. 만약 message 함수에 바인드된 함수정의를
찾지 못하면 어떻게 될까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(no-such-a-func "hello world")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
위의 표현식을 evaluate하면 아래와 같은 에러가 발생한다.

    Debugger entered--Lisp error: (void-function no-such-a-func)
      (no-such-a-func "hello world")
      eval((no-such-a-func "hello world"))
      ....
      
이맥스는 에러가 발생하면 기본적으로 위와 같은 디버깅 정보를 출력한다. 에러
메시지는 아래에서 위로 (콜 스택처럼) 인터프리터가 evaluate하는 과정을
나타낸다. 즉, 위의 메시지는 주어진 표현식을 evaluate하는 과정에서
no-such-a-func에 바인드된 함수를 찾지 못함(void-function)을 의미한다.

다시한번 아래와 같은 표현식을 evaluate해보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(message no-such-a-var)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

그러면 다음과 같은 에러가 발생하는데,

    Debugger entered--Lisp error: (void-variable no-such-a-var)
      (message no-such-a-var)
      eval((message no-such-a-var))
      ...

우리가 처음으로 변수(variable)와 값(value)이라는 개념을 접했다. 다양한 언어에
노출된 우리에게는 당연하게 생각될 수도 있는 간단한 개념이지만, 위의 에러메시지를
통해서 함수에 이름을 할당하듯 값에 이름을 할당할 수 있음을 알 수 있다. 위의
에러메시지를 다시 살펴보면, 우리가 주어진 표현식을 evaluate 하는 과정에서
no-such-a-var 심볼에 해당하는 값을 찾을 수 없음을 의미한다.

이 간단한 실험들은 몇가지 중요한 Lisp의 특성들을 알려주는데, 아래의 두 라인의
코드를 비교해서 생각해 보자. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(message no-such-a-var)
(message "hello world")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. "hello world": 값 (value)
1. no-such-a-var: 변수(variable)를 지칭하는 심볼
1. message: 함수(function)를 지칭하는 심볼
1. no-such-a-var의 심볼이 evaluate 된후 message 함수가 호출 (call-by-value)

즉, message와 no-such-a-var과 같은 심볼(이름)을 사용할때는 '이름'자체를
사용한다는 의미가 아니라 이름에 해당되는 값, 함수로 대채된 후 evaluate 됨을
기억하자. 그러면 아래의 표현식은 어떠한 에러가 날까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(message message)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

앞선 실험에서 message라는 심볼(이름)은 화면에 출력하는 함수에 바인딩되어 있음을
알아보았다. 위의 표현식은 message 함수를 함수를 인자로 부르는 것일까?

    Debugger entered--Lisp error: (void-variable message)
      (message message)
      eval((message message))
      ...

신기하게도 첫 message 심볼은 함수로, 두번째 message 심볼은 변수로 evaluate 하려고
하는것을 볼수있다. 이번 실험을 통해서 알려주고 싶었던 중요한 개념은 Lisp은 심볼의
"위치"에 따라 심볼을 함수로 볼지, 변수로 볼지 결정한다! 즉, 표현식의 가장 앞서
나오는 message 라는 심볼은 함수로 여겨지고, 그 이외의 위치에 나타나면 변수로
여겨진다.

사실 이제것 "표현식"이라고 지칭했던 것은 Lisp의 코드를 "리스트"의 형태로 나타낸
것이다! 필자가 이절을 통해서 말하고 싶은것은 세가지다. 

1. 심볼은 함수 또는 값을 갖는다.
2. 심볼을 어떻게 해석할지는 사용된 위치에 따라 결정된다.
3. 표현식은 심볼들의 리스트다.

# 리스트 (List)

자 리스트가 무엇인지? 어떻게 표현하고 해석하는지? 차근 차근 알아보자. 먼저 Lisp은
__LIS__t __P__rocessing의 약자이다. 즉 Lisp은 리스트를 하나 하나 해석하는
언어다.

    +---- 리스트의 시작
    v
    (message "width: %d" fill-column)
       인자1      인자2      인자3    ^
                      리스트의 끝 ----+

자 Lisp에서 message 심볼은 바인딩된 함수나 값을 의미함을 알았다. 그러면 어떻게
함수나 값이 아니라 심볼 자체를 나타낼 수 있을까? 자 *scratch* 버퍼로 이동 후
아래와 같이 입력하고 evaluate 해본다. (\k{C-j}를 message 끝에서 입력한다.)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
message
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

우리가 친숙하게 보았던 "message 변수가 정의 되지 않았다"는 에러메시지를 볼 수
있다. 그러면 **'**message를 입력후 (작은 따옴표 '가 앞에 붙었다.) evaluate
해본다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
'message
message
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

작은 따옴표 하나가 의미하는것들을 음미?해 보자. 첫째. 'message는 evaluate 하면
message의 함수나 값 대신 message 심볼을 리턴한다. 둘째. 당장 표현식을 evaluate
하는 대신 표현식을 수정하거나? evaluate 시점을 필요한 때로 미룰수 있게 되었다!

자자 이것을 이해하는 것이 필자에게도 참어려운 과정이었다. 대학교에서 scheme을
처음 접했던 순간을 떠올려 보면, 참 이해하기 힘들었던 개념인것 같다. 그러니 참고
아래와 같은 실험을 해보자.

가장 적은 수의 element을 갖는 비어 있는 (0개의 element을 갖는) 리스트는 어떻게
표현 할까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
()                                      ; empty list
nil                                     ; nil == (), ground or nil/null
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

비어 있는 리스트는 "()" 또는 동일하게 특별한 심볼인 nil로 나타낸다. 비어있음을
나타냄과 동시에 리스트의 끝을 나타내기도 한다. 자 그럼 element가 하나 있는
리스트는 아래와 같이 만들 수 있다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(cons 1 ())                             ; one element
(1)                                     ; 1 -> nil
(cons 1 (cons 2 ()))                    ; two elements
(1 2)                                   ; 1 -> 2 -> nil
(cons 1 (cons 2 (cons 3 ())))           ; three elements
(1 2 3)                                 ; 1 -> 2 -> 3 -> nil
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cons함수(__cons__tructor)는 (\k{C-h f}으로 찾아보자.) Lisp에서 리스트를 만드는
특별한 (special forms) 함수다. car, cdr로 지칭되는 두개의 인자를 받고, 두 인자를
묶어 (cons) 리턴한다.

    cons is a built-in function in `C source code'.

    (cons CAR CDR)

    Create a new cons, give it CAR and CDR as components, and return it.

즉 (cons 1 ())는 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(cons 1 2)
(1 . 2)

(car (cons 1 2))
1
(cdr (cons 1 2))
2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(cons 'message ())
(message)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.cl}
(message "width %d" fill-column)
"width 80"
(eval (cons 'message '("width %d" fill-column)))
"width 80"
(eval (list 'message '"width %d" 'fill-column))
"width 80"
(eval '(message "width %d" fill-column))
"width 80"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 그렇다면 주어진 리스트(표현식)를 해석(evaluate)하는 과정은, 첫 인자인
심볼(이름)을 함수로 해석(evaluate)하여 함수 정의를 가지고 오고, 

# 환경 (Environment)

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
