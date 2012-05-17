%
% Taesoo Kim
%
% title: 프로그래밍하기: Python
%
% abstract: 
% abstract: 
% abstract: 
%

7장과 8장을 통해 컴파일형 언어인 C언어를 사용하여 프로그래밍하는 방법을
알아보았다. 특히 8장에서는 이맥스가 다른 프로세스와 통신하는 방법을 알아보았고,
이를 활용하여 man, cpp, gcc 와 이맥스가 어떻게 상호작용하는지
살펴보았다. 이번장에서는 인터프리터형 언어로 대표되는 Python 언어를 어떻게
이맥스에서 사용하며, 점진적(incrementally)으로 개발하는, REPL 개발 방법론을
어떻게 적용할지 알아볼 것이다.

# 코드 작성하기

우리가 작성할 코드는 7장~8장에서 작성했던 시저암호화 프로그램이다. 이번장을 더
읽기 전에 직접 한번 작성해 보도록 하자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.py}
\!{cat enc.py}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

먼저 \k{C-x C-f: find-file}로 파일을 열고 `enc.py`을 입력하자. 이맥스는 몇가지
힌트와 추측을 통하여 파일에 (정확히는 파일에 해당하는 버퍼) 해당하는 주 모드를
결정한다. 첫번째는 파일의 확장자, C언어에서는 `enc.c`의 `.c`, `enc.py`의 `.py`를
보고 추측할 수 있겠다. \k{C-h v: help variable}을 통해서 \v{auto-mode-alist:
자동 주모드 리스트} 변수의 값을 살펴보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
auto-mode-alist is a variable defined in `files.el'.
...
Value: (
 ...
 ("\\.cs$" . c++-mode)
 ("\\.CPP$" . c++-mode)
 ...
 ("\\.[ch]\\'" . c-mode)
 ...
 ("\\.py\\'" . python-mode)
 ...
 (".*emacs-book/chap[0-9]+/.*.md$" . emacs-book-mode)
 (".*/linux-[0-9]\\.[0-9]+\\.[0-9]+*/.*\\.[ch]$" . linux-c-mode))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`alist` 타입의 \t{did I mention alist before?} `auto-mode-alist`는 `car`은 파일
이름에 해당하는 정규식과 `cdr`의 해당 주 모드 심볼의 리스트이다. 사용자가 파일을
열면 파일이름을 바탕으로 일치하는 주 모드를 검색한다. 즉 `.c` 파일은 `c-mode`로
`.py`파일은 `python-mode`로 주 모드가 설정된다.

재미있는것은 필자가 emacs-book의 각 챕터 디렉토리의 파일들을 열면 이맥스 책을
작성하기에 편한 모드로 세팅하도록 되어 있고, 리눅스 커널 디렉토의 파일을 열면
리눅스 커널 소스를 작업하기에 편한 모드를 같이 읽어 들이도록 설정되어 있다. 

두번째 이맥스가 주어진 파일에 대당하는 주 모드를 결정하는 것은, 파일의 첫라인의
인터프리터 선언문이다. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.py}
#!/usr/bin/python
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

파일에 python을 인터프리터로 선언한 경우, 이맥스는
\v{auto-mode-interpreter-regexp: 인터프리터 선언 정규식} 변수를 통해 해당하는
주모드를 추측한다. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
auto-mode-interpreter-regexp is a variable defined in `files.el'.
Its value is "#![ 	]?\\([^ 	\n]*/bin/env[ 	]\\)?\\([^ 	\n]+\\)"
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

즉 "#!/usr/bin/env python" 과 "#!/usr/bin/python"은 모두 "python"을 키위드로
\v{interpreter-mode-alist: 인터프리터 주모드 리스트} 변수를 통해 앞서 살펴본
\v{auto-mode-alist}와 같은 방법으로 주 모드를 결정한다. 이 변수는 놀랍지 않게
아래와 같은 값을 가지고 있다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interpreter-mode-alist is a variable defined in `files.el'.
...
Value: (
 ("runhaskell" . haskell-mode)
 ...
 ("ruby" . ruby-mode)
 ...
 ("python" . python-mode)
 ...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

마지막으로 파일의 시그니처인 magic값을 바탕으로 주모드를 추측하는
\v{magic-mode-alist: 내용 주모드 리스트}가 있다. 주로 파일 앞부분에 시그니처가
분명한 'ps', 'xml' 등의 파일들의 주모드가 이 리스트를 통해 결정된다.

# 도움말 호출하기

차 첫째 줄을 작성했으니 반은 이루었고, 다음 줄을 살펴보자. `enc.py`에서 두가지의
모듈, `os`와 `sys`을 사용한다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.py}
import os
import sys
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Python의 import 문은 어떠한 규칙을 가지고 있을까? 또 "os" 모듈은 어떠한
함수들을 제공하는가? 이를 알아보기 위해서는 각각의 문자열 위에서 \k{C-c C-f:
python-describe-symbol}을 호출해 보자. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Help on module os:

NAME
    os - OS routines for Mac, NT, or Posix depending on what system we're on.

MODULE REFERENCE
    http://docs.python.org/3.2/library/os
...    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

import문과, os와 sys 모듈은 정적으로 (문자적으로) 도움말을 찾아 보여줄 수
있다. 하지만 동적으로 코드의 의미가 결정되는 Python은 완벽하게 개발자의 

# Python 인터프리터 호출하기




- inferior (comint)
- maps
  C-c </>
  C-c C-f: describe symbol
  C-c C-v: python-check
  C-c C-s: send string
  C-M-x  : send defun
  C-c C-r: send region
  C-c M-r: send region & go
  C-c C-c: send buffer
  C-c C-z: python
  C-c C-l: load filel
  pdb
  next/prev-error

- flymake (pylint/pycheck)
- eldoc/imenu
- hippie expand
- pymacs
- eshell/grep

=> installing pylookup
=> chap10
