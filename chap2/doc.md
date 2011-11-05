%
% Taesoo Kim
%

# Lisp 이해하기

Lisp의 재미있는 특성들을 하나의 장에서 모두 다루기는 불가능하다. 이번장에서는
이멕스를 확장하고 이해하는데 필요한 기본적인 특성들을 알아보자.

## 심볼 (Symbol)

아래와 같은 표현식을 다시 살펴보자.

    (message "hello world")

위 표현식은 message라는 함수를 "hello world" 문자열을 인자로 호출하라는
의미이다. 여기서 **message**는 화면 하단에 문자열을 출력하는 함수의 이름이다. 즉
message는 함수정의를 *지칭하는* 용도로 사용되고 있다. 뻔한? 소리를 이렇게
강조하는 이유는 Lisp에서는 명시적으로 이름(앞으로 **심볼**)과 함수정의 (또는
변수값)을 분리해서 쓴다. 그렇게 함으로써 언어 특성이 간단해 지고 코드를
데이터처럼 사용할 수 있게 된다. (앞으로 살펴 볼 것이다.)

Lisp 인터프리터가 위의 표현식을 해석하는 과정을 상상해보자. 먼저 문자열로 주어진
표현식을 받은후, 파싱을 하고, message 심볼에 바인딩(bound)된 함수정의를 찾고
주어진 인자와 함께 호출할것이다. 만약 message에 바인드된 함수정의를 찾지 못하면
어떻게 될까?

    (no-such-a-func "hello world")

위의 표현식을 evaluate하면 아래와 같은 에러가 발생한다.

    Debugger entered--Lisp error: (void-function no-such-a-func)
      (no-such-a-func "hello world")
      eval((no-such-a-func "hello world"))
      ....
      
이멕스는 에러가 발생하면 기본적으로 위와 같은 디버깅 정보를 출력한다. 에러
메시지는 아래에서 위로 (스택처럼) 인터프리터가 evaluate하는 과정을 나타낸다. 즉,
위의 메시지는 주어진 표현식을 evaluate하는 과정에서 no-such-a-func에 바인드된
함수를 찾지 못함(void-function)을 의미한다.

다시한번 아래와 같은 표현식을 evaluate해보자.

    (message no-such-a-var)

그러면 다음과 같은 에러가 발생하는데,

    Debugger entered--Lisp error: (void-variable no-such-a-var)
      (message no-such-a-var)
      eval((message no-such-a-var))
      ...

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

## 리스트 (List)

Lisp은 LISt Processing의 약자이다. 즉 

## Special Forms

- evaluate
- calling function
- control
- loop
- define function

- special forms
 - if/else
 - let
 - defun (define function)
