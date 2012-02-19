%
% Taesoo Kim
%
% title: 프로그래밍하기: C언어-2
%
% abstract: C언어로 프로그래밍을 위해 이맥스가 어떠한 편리한 기능들을
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

# 프로세스 호출 하기

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

그럼 어떤 프로세스들을 이멕스가 관리하고 있는 것일까? \f{M-x list-process}를 실행
해보자.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.scheme}
(let ((buf (generate-new-buffer "DATE")))
  (set-process-sentinel
   (start-process "date" buf "date")
   (lambda (proc out) (message "Done!"))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(start-process 

- compilation
  regexp
  child process invocation (async)
- gdb
  process control in emacs
- woman
- c-macro-expand
  sync process invocation

- tips:
  regexp-assign
  c-warn mode
  eshell

=> chap8: python/ruby or haskell (interpreter)
=> chap9: init (permanent)

응용예) 탭과 스페이스

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(setq-default indent-tabs-mode nil ; off tab mode
              tab-width        4   ; instead 4 spaces mode
              fill-column      80) ; fill width
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


show:
 whitespace-mode
 untabify

hook

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
(defun check-tab-mode ()
  "Determine tab mode and width"
  (interactive)
  (let ((width 4)
        (mode nil))
    (when (save-excursion
            (goto-char (point-min))
            (search-forward "\t" (min 3000 (point-max)) t))
      (setq width 8)
      (setq mode t))

    (setq indent-tabs-mode mode)
    (setq tab-width width)
    (when mode (message "Set 'tab-mode' with %d" width))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))
                


<!--  -->
현재 버퍼의 주/부 모드에 관한 정보 및 모드에 대한 키바인딩은 \k{C-h m} (**h**elp
**m**ode) 명령을 통해 살펴볼 수 있다.

C-c .

% snap
% C-h m

regexp
dabbrev-expand
man
ido-mode
comment-dwin
c-macro
c-up/down/next-conditional
cwarn-mode
highlight-parentheses-mode
highlight-current-line-on
flyspell-prog
flymake

# 모드 (Mode)

일반적으로 우리가 특정 파일을 이맥스에서 열었을때 이맥스는 열린 파일에 대한
버퍼를 생성하하고, "버퍼"의 이름에서 알 수 있듯이 사용자는 파일을 직접 수정하는
것 대신 버퍼를 수정하게 된다. 이때 현재 작업하는 버퍼(파일)에 대하여 특별히
유용한 함수들와 키맵(map)의 그룹을 불러 들이게 된다. 예를 들면 C언어로 짜여진
파일을 열었을 경우, C언어에 특화된 함수와 변수(코드 하이라이팅, 주석 처리 등)를
불러들이고, 이러한 함수와 변수를 하나로 모아서 모드(mode)라고 부른다. 즉 C언어로
짜여진 파일을 열면 (".c"나 ".h"의 확장자를 갖는 파일을 열면), 현재 버퍼의
`major-mode`가 `c-mode`가 된다. 버퍼를 기능별로 분류하다 보니, 자연스럽게 하나의
버퍼당 하나의 `major-mode`를 갖게 되며, 현재 버퍼의 모드는 `major-mode`의 변수에
기록이 된다. \f{C-h v: 변수 도움말}을 현재 버퍼의 주모드가 무엇인지 확인해 보도록
하자.


 - with a c example
 - editing/searching
 
 - modifying global-map (C-z -> undo or C-q -> kill-buffer)
 - explain init process

 => next chapter (tools, compile ...)
 => windowing
 => modifying local-map
 

ido 소개
-> permenant?
-> init file

기본틀 -> 설명?
- basic option feilds
- linux kernel mode?

모드
- .bashrc 파일 열어보기
- 개념
- 키 -> 함수
- .c

=> so a way to recognize this: auto-mode-alist

- jumping

    C-u C-SPC 
    M-g g (goto-line)
    M-g n (next-error)
    M-g p (previous-error)
    
- search

    C-s isearch-forward
    C-r isearch-backward

    C-M-s isearch-forward-regexp
    C-M-r isearch-backward-regexp
    
- editing

    DEL delete-backward-char
    M-DEL backward-kill-word
    C-DEL delete-kill-word
    C-d delete-char
    M-d kill-word
    M-k kill-line
    C-M-k kill-sexp
    M-z zap-to-char
    C-t 
    M-t 
    C-M-t
    C-o
    C-x C-o
    C-j
    C-space

- windowing

    C-x 1
    C-x 2
    C-x 3
    C-x 5
    C-M-v: scroll-other-window-up
    C-M-S-v: scroll-other-window-down
    
- register

=> end with how to modify global-map and permanently change it
                
