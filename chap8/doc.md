%
% Taesoo Kim
%
% title: 프로그래밍하기: C언어-2
%
% abstract: 이멕스가 프로세스를 어떻게 관리하고, 상호작용하는지, 또 이를 활용한 
% abstract: 기능들이 어떻게 사용되는지 C언어로 프로그래밍하면서 하나하나 
% abstract: 알아본다.
%

고전 "The Art of Unix Programming"에서 이멕스와 Vi의 소위 "Holy War"에 대한
이야기를 소개하고 있다. Eric S. Raymond에 따르면, 이멕스의 가장 큰 장점 중 첫째가
우리가 3장, 4장에서 알아본 확장성 있는 내장언어인, Lisp에 관한 이야기
였다. 두번째 장점은 이멕스가 프로그래머들이 즐겨 사용하는 다양한 외부 프로그램과
손쉽게 통합되어 이멕스를 떠나지 않고 활용할 수 있다는 점이었다. 이번 장에서는
이멕스가 가지고 있는 확장성, 외부 프로그램과의 커뮤니케이션에 집중하여 7장에서
부터 만들고 있는 시져 암호화 프로그램을 마무리 지어 보자.

# Man vs Woman

지적 호기심을 해결 하고싶은 마음, 궁금증을 해결하고 싶은마음은 프로그래머로
갖추어야할 기본적인 아니 필수적인 자세이다. 언어의 특성, 큰 그림, 기반한
아키텍쳐와 환경을 이해 했다면 그 언어를 전통할 수 있는 가장 중요한 '리소스'는
쉽게 이미 준비되고 테스트된 모듈, 클레스, 함수를 재활용하는 것이다. 이러한
이유해서 이멕스를 배우기에 가장 쉬운 방법이 1장, 2장을 통해 설명한 도움말에 쉽게
접근하는 것이다.

C프로그래밍을 하다 모르는 함수가 나오면, 모호하게 사용되고 있는 상수들이 나오면
어떻게 하고 있는가? 리눅스/유닉스 사용자라면 잘 알려진, 아니 너무 자세하고
불?친절한, `man` (manual) 명령을 통해 문서를 찾아 읽어 볼 것이다. 일반적으로
`man` 페이지를 튜토리얼 처럼 생각하고 읽으면, 더없이 불필요하고 딱딱한
문서이다. 그런데 레퍼런스로 생각하고 읽으면 더없이 잘 정돈되어 있고, 간략하게
느껴지는 도움말이기도하다.

POSIX 표준이 아니라, `err()` 어떠한 함수인지 모르는 독자들이 있을 것이다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
int main(int argc, char *argv[]) {
  if (argc != 2) {
    []err(1, "usage: %s [text]\n", argv[0]);
  }
  ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

자 그럼 `err()`에 커서를 놓고 \k{M-x man}을 입력해보자. 다른 창에 해당하는
도움말이 나타났는가? 그러면 해당 버퍼 '\*Man err\*'로 이동해서 `TAB`을 입력해
볼까? 오랜 리눅스 사용자였다면 기본 `man` 페이지에 있는 다른 링크들(SEE ALSO
색션)을 보고 관련된 명령을, `man`을 종료한후 다시 입력하는 습관을 가지고
있을것이다. 신기하게도 `man`에서는 그러한 기능이 없다.  이멕스는 Lisp으로 짜여진
더욱 편하고, 아름다운? `woman`이 있다. 관심있는 독자는 실행해 보고, 문서가 눈에
더 잘들어오도록 설정해 보기바란다.

자 그럼! 사용자가 이멕스의 \f{man} 리습함수를 호출 하면, 이멕스에서는 어떠한 일이
일어나고 있는 것일까?

# 자식 프로세스 만들기

\f{man}의 함수정의를 찾아 가볼까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(defun man (man-args)
  "Get a Un*x manual page and put it in a buffer. ...."

  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
		;; case-insensitive completition
		(completion-ignore-case t)
		(input (completing-read
			(format "Manual entry%s"
				(if (string= default-entry "")
				    ": "
				  (format " (default %s): " default-entry)))
                        'Man-completion-table
			nil nil nil 'Man-topic-history default-entry)))
	   (if (string= input "")
	       (error "No man args given")
	     input))))

  ;; translate the "subject(section)" syntax
  (setq man-args (Man-translate-references man-args))

  (Man-getpage-in-background man-args))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

당황해 하지 말고, 다시 위로 돌아가서 함수의 정의를 차근차근 읽어보자. 모두
우리가 이미 살펴본 문법, 함수들을 사용하고있다. 위의 함수가 아래와 같이 눈에
들어와야 할것이다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(defun man (man-args)
  "설명이군"

  ;; 인자를 받네, 자동완성/히스토리도 지원하군.
  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
        ;; 무시!
        (completion-ignore-case t)
        (input (completing-read
            (format "Manual entry%s"
                (if (string= default-entry "")
                    ": "
                  (format " (default %s): " default-entry)))
                        'Man-completion-table
            nil nil nil 'Man-topic-history default-entry)))
       ;; 인자가 없으면 안되겠지?
       (if (string= input "")
           (error "No man args given")
         input))))

  ;; 내가 입력한 인자를 왜 덮어쓰지? 설명이 잔뜩있네.
  ;; 아하 "3 err"말고도 "err(3)" 이렇게 입력해도 된다네!
  (setq man-args (Man-translate-references man-args))

  ;; 드디어 'man'을 호출하고 버퍼로 가져 오구나.
  (Man-getpage-in-background man-args))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`man`의 기능은 해당하는 문서파일을 찾아 압축을 풀고, 렌더링을 해서 화면에
출력하는 것이다. "SEE ALSO" 섹션의 관련 명령들은 '링크'라기보다 꾸며진 '글자'라고
보는것이 맞을 것이다. 이멕스의 `man`에서 하는 일은 SEE ALSO 섹션에 나열되어있는
글자들도 `man`이 이해하는 인자로 변경하고, `man`을 호출하여 문서를 버퍼로 가져온
다음 버퍼의 필요한 곳곳에 링크를 만들었다.

그러면 `man` 함수의 핵심(gist!)인 \f{Man-getpage-in-background}를 살펴볼까?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
;; 자 함수를 읽기전에, 함수가 무슨일을 할까? 잠시 생각해보자. 아마도 버퍼를
;; 새로 만들고, man 프로세스를 호출하고 .. 아! background로 프로세스를 실행하면 
;; 함수는 바로 리턴이 될테고, 누군가 프로세스를 감시하고 있어야 될텐데?
;;
(defun Man-getpage-in-background (topic)

  (let* ((man-args topic)
         (bufname (concat "*Man " man-args "*"))
         (buffer  (get-buffer bufname)))
         
      ;; 역시 버퍼를 만드는군
      ...
      (message "Invoking %s %s in the background" manual-program man-args)
      (setq buffer (generate-new-buffer bufname))
      (with-current-buffer buffer
        (setq buffer-undo-list t)
        (setq Man-original-frame (selected-frame))
        (setq Man-arguments man-args))
        
      ;; 아 맞아. man이 내 터미널에 환경을 알아야 꾸며 주지! 맞아 맞아 터미널을
      ;; 줄여도 자기가 막 알아서 조절했었지 ...
      (let ((process-environment (copy-sequence process-environment)))
       ....
        ;; 머 이것저것 많이 하네 .. 터미널 타입, 스크린크기 머 그쯤 아닌가 ..
        (setenv "TERM" "dumb")
        (unless (or (getenv "MANWIDTH") (getenv "COLUMNS"))
          (setenv "COLUMNS" (number-to-string
          ...
        (setenv "MAN_KEEP_FORMATTING" "1")
        
        ;; 아하! 여기다
        (if (fboundp 'start-process)
            ;; 친절하기도 하시지 cygwin, windows 환경도 고려하네 하하
            ;; 딱보니 'sh -c man err' 정도를 호출하구만 ...
            (set-process-sentinel
             (start-process manual-program buffer
                            (if (memq system-type '(cygwin windows-nt))
                                shell-file-name
                              "sh")
                            shell-command-switch
                            (format (Man-build-man-command) man-args))
             ;; 아 .. 이게 함수 심볼인가? 이걸 asynchronous하게 호출 하나?
             'Man-bgproc-sentinel)
             ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

코드를 훑어 보았는가? 우리를 흥분되게 만드는 두개의 함수
\f{set-process-sentinel}와 \f{start-process}를 살펴보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
;; 이름(닉네임), 버퍼(프로그램의 stdin/out), 프로그램, 인자들 ...
(start-process "gedit" nil "gedit" "/etc/passwd")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

이렇게 간단하게 프로그램을 호출 할수 있다. 첫 인자 '이름'은 이멕스 내부의 PID라고
생각할 수 있다. 프로세스 고유의 이름으로, 우리가 앞으로 "gedit"라고 호명하면
이멕스는 지금 실행될 프로세스를 지칭하는지 이해할 수 있다.

그럼 어떤 프로세스들을 이멕스가 관리하고 있는 것일까? \f{M-x list-processes}를 실행
해보자.

    Proc  Status   Buffer Tty	      Command
    ----  ------   ------ ---	      -------
    gedit run      (none) /dev/pts/5  gedit /etc/passwd

이멕스가 `gedit` 에디터를 실행과 종료의 프로세스 상태, 입/출력을 관리하고 있음을
알수있다. 그러면 어떻게 프로세스를 종료 할까? \f{kill-process: 프로세스
종료하기}와 우리가 생성할때 명명한 닉네임 "gedit"을 이용하면 실행한 프로세스를
종료 할 수 있다. 아래를 실행시켜 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(kill-process "gedit")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

프로세스는 생각보다? 쉽게 생성할 수있었다. 그러면 어떻게 자식 프로세스가
종료되는지 알 수 있을까?

# 프로세스 이벤트

먼저 프로세스가 종료하면 (이벤트가 발생하면), 특정 함수를 호출하게 해보자. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
;; DATE 이름의 버퍼를 생성
(let ((buf (generate-new-buffer "DATE")))
  ;; 프로세스를 감시할 함수를 지정
  (set-process-sentinel
    ;; 생성할 프로세스
    (start-process "date" buf "date")
    ;; 프로세스의 상태가 바뀔때 마다,
    ;; 프로세스 오브젝트(proc)와 상태 문자열(out)을 인자로 호출
    (lambda (proc out) (message "Done!"))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 코드를 실행하면 "Done"의 메시지가 출력된다. 우리가 명명한 "DATE"의 버퍼를
살펴볼까? 현재 시간이 기록되어 있는가? 이멕스는 버퍼를 입/출력 단위로
사용한다. 왜 버퍼를 사용할까? 이미 이멕스는 수많은 기능들 버퍼를 수정하고,
꾸미고, 관리하는 기능들을 가지고 있는데, 버퍼의 입출력은 특별히 이러한 기능들을
쉽게 재사용하능하게 한다. 

"man" 함수에서 볼 수 있듯이, 출력을 버퍼로 받아와서 꾸미고, 링크를 만들고,
바인딩을 변경하는 일들이 특별히 쉬워진다. 이제 외부 프로세스를 제어 할 수
있으니, 할 수 있는 일들이 무궁무진 해졌다.

# CPP 호출 하기 (프리프로세싱)

C프로그래밍을 하다보면 소위 "macro"라고 불리우는 "#define"문이 절실히 필요할 때가
많다. 하지만 남용되어져 프로그래밍 컨택스트를 잃어버리기 마련이다. 컴파일러는
소스를 컴파일 하기 전에 소스를 `cpp` 프로그램을 통해 프리프로세싱하여
"#include"와 "#define"등을 문자열로 변경한다. 이멕스에서 특정 줄의 표현식의
프리프로세싱된 결과를 볼 수 있는 기능이 있다. 

아래의 두줄을 선택하고 \k{C-space: 선택 시작} 이후 커서를 움직여 원하는 영역을
선택하고, \k{M-x c-macro-expand}를 실행하자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
  const char *encrypted = ceaser(argv[1], SHIFT, PRIME);
  const char *decrypted = ceaser(encrypted, -SHIFT, PRIME);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

아래와 같이 SHIFT와 PRIME이 우리가 정한 값으로 변경되었다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
  const char *encrypted = ceaser(argv[1], (11), (17));
  const char *decrypted = ceaser(encrypted, -(11), (17));
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

이 과정이 얼마나 단순한지 상상 할 수 있겠는가? 해당 파일을 `cpp`로 호출 한 후,
선택된 줄을 찾아 버퍼에 출력한다. 소스코드의 하이라팅을 위해 버퍼의 주모드를
`c-mode`로 설정하면 된다.

# 컴파일 하기

나머지 시져 암호화 프로그램을 완성한 후, 컴파일하기 위해 \k{M-x compile}을
실행하자. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
make -k []
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(`make`의 "-k" 옵션은 `make`의 컴파일 과정에서 오류가 발생해도 끝까지 dependency
그래프를 traverse 하라는 의미이다.\todo{traverse in korean})

`make`의 기본 규칙을 활용해서, 실행파일의 이름 "enc"을 입력하고 현재 소스코드를
컴파일 해본다. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
make -k enc
cc     enc.c   -o enc
enc.c: In function ‘ceaser’:
enc.c:15:26: warning: incompatible implicit declaration of built-in function ‘malloc’
enc.c:17:3: error: ‘for’ loop initial declarations are only allowed in C99 mode
enc.c:17:3: note: use option -std=c99 or -std=gnu99 to compile your code
make: *** [enc] Error 1

Compilation exited abnormally with code 2 at Tue Feb 21 02:55:55
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

더 컴파일 과정을 진행하기 전에, 우리가 어떤일을 하고 있는지 알아보자. "make"
프로세스를 호출 했고, 해당하는 결과를 버퍼로 받아 왔다. 컴파일 결과가 담긴
버퍼의 오류가난 줄들을 하이라이팅했고(face), 링크를 만들었다. `man`에서와
마찬가지로 `regexp`\todo{korean}를 활용해서 특정 패턴을 찾고, 해당하는 face를
만들어 하이라이팅 한것이다. 별로 특별한 것이 하나도 없다.

그러면 오류가 난 곳으로 이동하기 위해서는 아래와 같은 명령을 활용한다.

- \k{M-g n: go next error}: 오류가 난 다음 줄로 이동
- \k{M-g p: go previous error}: 오류가 난 이전 줄로 이동

오류는 우리가 다소 최신(?) 표준인 `for`문 안에 변수를 선언했기 때문인데, 다음과
같은 명령으로 컴파일 할 수 있다. \k{M-x compile} 이후 "CFLAGS=-std=c99 make -k
enc"를 입력해서 프로그램을 컴파일 해 본다.

# 프로그램 실행하기

모두 같은 맥락으로, 컴파일된 프로그램을 실행 할 수도 있다. \k{C-!} 또는 \k{M-x
shell-commnad}를 실행해본다. 그리고 프롬프트가 나오면 아래와 같이 입력한다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.c}
./enc good
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

출력 창에 아래와 같은 이쁜? 이모티콘을 볼 수 있을 것이다.

    'good' =enc->> 'aiio' =dec->> 'V^^d'

# 프로세스와 상호작용하기

자 그러면 한발 더 나아가서, 어떻게 프로세스와 상호작용할까? 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(let ((buf (generate-new-buffer "PYTHON")))
  (start-process "python" buf "python" "-i"))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

파이선 프로세스를 생성시키고, "PYTHON" 버퍼로 이동해 볼까? 우리가 파이선의 초기화
메시지들이 출력되었는가? (참고로 "-i" 옵션은 "interactive" 모드로 출력을 버퍼링
하지 않는다.)

그리고 \f{list-processes}를 실행해 보면, 파이선 프로세스가 실행되고 있음을 알 수
있다. 자 파이선을 제어하기 위해서는 \f{process-send-string: 프로세스에 문자열
보내기}와 \f{process-send-eof: 프로세스의 종료문자 보내기}을 활용하면
된다. 아래의 예를 보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(process-send-string "python" "print 'hello world from emacs!'\n")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 예제를 실행하고 "PYTHON" 버퍼로 이동하면 "hello world from emacs!"가
출력되었음을 알 수 있다. 아하! 파이선과 같은 인터프리터형 프로그램을 위와 같이
제어 하면, 내가 프로그래밍하면서 현재 버퍼의 함수/클레스를 보내 파이선
인터프리터에 보내 실행해 볼 수 있지 않을까? 이와 관련된 궁금증이 바로 우리가
다음장에서 같이 해결해 볼 내용이다.

